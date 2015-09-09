package rescala.propagation

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph.{Spores, Committable, Reactive}
import rescala.turns.Turn

import scala.util.Try

trait PropagationImpl[S <: Spores] extends Turn[S] {
  implicit def currentTurn: PropagationImpl[S] = this

  private var toCommit = Set[Committable]()
  private var observers = List.empty[() => Unit]
  private var _evaluated = List.empty[Reactive[S]]

  val levelQueue = new LevelQueue()

  def evaluate(head: Reactive[S]): Unit = {
    def requeue(changed: Boolean, level: Int, redo: Boolean): Unit =
      if (redo) levelQueue.enqueue(level, changed)(head)
      else if (changed) head.outgoing.get.foreach(levelQueue.enqueue(level, changed))

    head.reevaluate() match {
      case Static(hasChanged) =>
        requeue(hasChanged, level = -42, redo = false)
      case Dynamic(hasChanged, diff) =>
        diff.removed foreach unregister(head)
        diff.added foreach register(head)
        val newLevel = maximumLevel(diff.novel) + 1
        requeue(hasChanged, newLevel, redo = head.level.get < newLevel)
    }
    _evaluated ::= head

  }

  def maximumLevel(dependencies: Set[Reactive[S]]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.level.get))

  def register(sink: Reactive[S])(source: Reactive[S]): Unit = source.outgoing.transform(_ + sink)

  def unregister(sink: Reactive[S])(source: Reactive[S]): Unit = source.outgoing.transform(_ - sink)

  override def schedule(commitable: Committable): Unit = toCommit += commitable

  override def observe(f: => Unit): Unit = observers ::= f _

  override def create[T <: Reactive[S]](dependencies: Set[Reactive[S]], dynamic: Boolean)(f: => T): T = {
    val reactive = f
    val level = ensureLevel(reactive, dependencies)
    if (dynamic) evaluate(reactive)
    else {
      dependencies.foreach(register(reactive))
      if (level <= levelQueue.currentLevel() && dependencies.exists(_evaluated.contains)) {
        evaluate(reactive)
      }
    }
    reactive
  }

  def ensureLevel(dependant: Reactive[S], dependencies: Set[Reactive[S]]): Int =
    if (dependencies.isEmpty) 0
    else {
      val newLevel = dependencies.map(_.level.get).max + 1
      dependant.level.transform(math.max(newLevel, _))
    }

  override def admit(reactive: Reactive[S]): Unit = levelQueue.enqueue(reactive.level.get)(reactive)
  override def forget(reactive: Reactive[S]): Unit = levelQueue.remove(reactive)

  /** allow turn to handle dynamic access to reactives */
  override def accessDynamic(dependency: Reactive[S]): Unit = ()

  def lockPhase(initialWrites: List[Reactive[S]]): Unit

  def propagationPhase(): Unit = levelQueue.evaluateQueue(evaluate)

  def commitPhase() = toCommit.foreach(_.commit(this))

  def rollbackPhase() = toCommit.foreach(_.release(this))

  def observerPhase() = {
    val executed = observers.map(o => Try {o.apply()})
    // find the first failure and rethrow the contained exception
    // we should probably aggregate all of the exceptions,
    // but this is not the place to invent exception aggregation
    executed.find(_.isFailure).foreach(_.get)
  }

  def releasePhase(): Unit

  var collectedDependencies: List[Reactive[S]] = Nil

  def collectDependencies[T](f: => T): (T, Set[Reactive[S]]) = {
    val old = collectedDependencies
    collectedDependencies = Nil
    val res = (f, collectedDependencies.toSet)
    collectedDependencies = old
    res
  }
  def useDependency(dependency: Reactive[S]): Unit = collectedDependencies ::= dependency

}
