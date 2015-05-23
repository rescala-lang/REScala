package rescala.propagation

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph.{Committable, Reactive}
import rescala.turns.Turn

object QueueAction {
  
  sealed abstract class QueueAction
  case object EnqueueDependencies extends QueueAction
  case object RequeueReactive extends QueueAction
  
}

trait TurnImpl extends Turn {
  import QueueAction._
  
  implicit def currentTurn: TurnImpl = this

  protected[this] var toCommit = Set[Committable]()
  private var observers = List.empty[() => Unit]

  val levelQueue = new LevelQueue()

  protected def requeue(head: Reactive,changed: Boolean, level: Int, action: QueueAction): Unit = action match {
    case EnqueueDependencies => if (changed) head.outgoing.get.foreach(levelQueue.enqueue(level, changed))
    case RequeueReactive => levelQueue.enqueue(level, changed)(head)
  }
  
  /**
   * Evaluates the the given Reactive and requeues it.
   * @return whether the reactive itself is requeued or its dependencies
   */
  def evaluate(head: Reactive): QueueAction = {
    val result = head.reevaluate() 
    val (hasChanged, newLevel, action) = result match {
      case Static(hasChanged) =>
        (hasChanged, -1, EnqueueDependencies)
      case Dynamic(hasChanged, diff) =>
        diff.removed foreach unregister(head)
        diff.added foreach register(head)
        val newLevel = maximumLevel(diff.novel) + 1
        val action = if (head.level.get < newLevel) RequeueReactive else EnqueueDependencies
        (hasChanged, newLevel,  action)
    }
    requeue(head, hasChanged, newLevel, action)
    action
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

  // TODO Need to synchronize the queue? I thinkg it is necessary
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
