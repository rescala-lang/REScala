package rescala.propagation

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph.{Committable, Reactive}
import rescala.turns.Turn

trait PropagationImpl extends Turn {
  implicit def currentTurn: PropagationImpl = this

  private var toCommit = Set[Committable]()
  private var observers = List.empty[() => Unit]

  val levelQueue = new LevelQueue()

  def evaluate(head: Reactive): Unit = {
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

  }

  def maximumLevel(dependencies: Set[Reactive])(implicit turn: Turn): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.level.get))

  def register(sink: Reactive)(source: Reactive): Unit = source.outgoing.transform(_ + sink)

  def unregister(sink: Reactive)(source: Reactive): Unit = source.outgoing.transform(_ - sink)

  override def schedule(commitable: Committable): Unit = toCommit += commitable

  override def observe(f: => Unit): Unit = observers ::= f _

  override def create[T <: Reactive](dependencies: Set[Reactive], dynamic: Boolean)(f: => T): T = {
    val reactive = f
    ensureLevel(reactive, dependencies)
    if (dynamic) evaluate(reactive)
    else dependencies.foreach(register(reactive))
    reactive
  }

  def ensureLevel(dependant: Reactive, dependencies: Set[Reactive]): Int =
    if (dependencies.isEmpty) 0
    else {
      val newLevel = dependencies.map(_.level.get).max + 1
      dependant.level.transform(math.max(newLevel, _))
    }

  override def admit(reactive: Reactive): Unit = levelQueue.enqueue(reactive.level.get)(reactive)
  override def forget(reactive: Reactive): Unit = levelQueue.remove(reactive)

  /** allow turn to handle dynamic access to reactives */
  override def accessDynamic(dependency: Reactive): Unit = ()

  def lockPhase(initialWrites: List[Reactive]): Unit

  def propagationPhase(): Unit = levelQueue.evaluateQueue(evaluate)

  def commitPhase() = toCommit.foreach(_.commit(this))

  def rollbackPhase() = toCommit.foreach(_.release(this))

  def observerPhase() = observers.foreach(_.apply())

  def releasePhase(): Unit

}
