package rescala.propagation

import java.util

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph.{LevelStruct, Reactive}

import scala.util.control.NonFatal


trait LevelBasedPropagation[S <: LevelStruct] extends AbstractPropagation[S] {
  implicit def currentTurn: LevelBasedPropagation[S] = this

  private val toCommit = new util.HashSet[Committable]()
  private val observers = new java.util.ArrayList[() => Unit](20)
  private var _evaluated = List.empty[Reactive[S]]

  val levelQueue = new LevelQueue()

  def evaluate(head: Reactive[S]): Unit = {

    def requeue(changed: Boolean, level: Int, redo: Boolean): Unit =
      if (redo) levelQueue.enqueue(level, changed)(head)
      else if (changed) head.bud.outgoing.foreach(levelQueue.enqueue(level, changed))

    head.reevaluate() match {
      case Static(hasChanged) =>
        requeue(hasChanged, level = -42, redo = false)
      case Dynamic(hasChanged, diff) =>
        diff.removed foreach drop(head)
        diff.added foreach discover(head)
        val newLevel = maximumLevel(diff.novel) + 1
        requeue(hasChanged, newLevel, redo = head.bud.level < newLevel)
    }
    _evaluated ::= head

  }

  def maximumLevel(dependencies: Set[Reactive[S]]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.bud.level))

  def discover(sink: Reactive[S])(source: Reactive[S]): Unit = source.bud.discover(sink)

  def drop(sink: Reactive[S])(source: Reactive[S]): Unit = source.bud.drop(sink)

  override def schedule(commitable: Committable): Unit = toCommit.add(commitable)

  override def observe(f: => Unit): Unit = observers.add(f _)

  override def create[T <: Reactive[S]](dependencies: Set[Reactive[S]], dynamic: Boolean)(f: => T): T = {
    val reactive = f
    val level = ensureLevel(reactive, dependencies)
    if (dynamic) evaluate(reactive)
    else {
      dependencies.foreach(discover(reactive))
      if (level <= levelQueue.currentLevel() && dependencies.exists(_evaluated.contains)) {
        evaluate(reactive)
      }
    }
    reactive
  }

  def ensureLevel(dependant: Reactive[S], dependencies: Set[Reactive[S]]): Int =
    if (dependencies.isEmpty) 0
    else {
      val newLevel = dependencies.map(_.bud.level).max + 1
      dependant.bud.updateLevel(newLevel)
    }

  /** allow turn to handle dynamic access to reactives */
  override def dependencyInteraction(dependency: Reactive[S]): Unit = ()

  override def preparationPhase(initialWrites: List[Reactive[S]]): Unit = initialWrites.foreach { reactive =>
    levelQueue.enqueue(reactive.bud.level)(reactive)
  }

  def propagationPhase(): Unit = levelQueue.evaluateQueue(evaluate)

  def commitPhase() = {
    val it = toCommit.iterator()
    while (it.hasNext) it.next().commit(this)
  }

  def rollbackPhase() = {
    val it = toCommit.iterator()
    while (it.hasNext) it.next().release(this)
  }

  def observerPhase() = {
    val it = observers.iterator()
    var failure: Throwable = null
    while (it.hasNext) {
      try {
        it.next().apply()
      }
      catch {
        case NonFatal(e) => failure = e
      }
    }
    // find the first failure and rethrow the contained exception
    // we should probably aggregate all of the exceptions,
    // but this is not the place to invent exception aggregation
    if (failure != null) throw failure
  }

}
