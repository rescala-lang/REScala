package rescala.pipelining.propagation

import rescala.graph.{Reactive, ReevaluationResult}
import rescala.pipelining.PipelineStruct
import rescala.propagation.{Committable, AbstractPropagation, Turn}


private[pipelining] trait PipelinePropagationImpl extends AbstractPropagation[PipelineStruct.type] {

  type S = PipelineStruct.type

  implicit def currentTurn: PipelinePropagationImpl = this

  protected[this] var toCommit = Set[Committable]()
  private var observers = List.empty[() => Unit]

  val levelQueue = new PipelineQueue()

  protected def requeue(head: Reactive[S], changed: Boolean, level: Int, action: QueueAction): Unit = action match {
    case EnqueueDependencies =>  head.bud.outgoing.foreach(levelQueue.enqueue(level, changed))
    case RequeueReactive => levelQueue.enqueue(level, changed)(head)
    case DoNothing =>
  }

  protected def calculateQueueAction(head : Reactive[S], result: ReevaluationResult[S]) : (Boolean, Int, QueueAction)

  /**
    * Evaluates the the given Reactive and requeues it.
    *
    * @return whether the reactive itself is requeued or its dependencies
    */
  def evaluate(head: Reactive[S]): QueueAction = {
    val result = head.reevaluate()
    val (hasChanged, newLevel, action) = calculateQueueAction(head, result)
    requeue(head, hasChanged, newLevel, action)
    action
  }

  def maximumLevel(dependencies: Set[Reactive[S]])(implicit turn: Turn[S]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.bud.level))

  def register(sink: Reactive[S])(source: Reactive[S]): Unit = source.bud.discover(sink)

  def unregister(sink: Reactive[S])(source: Reactive[S]): Unit = source.bud.drop(sink)

  override def schedule(commitable: Committable): Unit = toCommit += commitable

  override def observe(f: => Unit): Unit = observers ::= f _

  override def create[T <: Reactive[S]](dependencies: Set[Reactive[S]], dynamic: Boolean)(f: => T): T = {
    val reactive = f
    ensureLevel(reactive, dependencies)
    if (dynamic) evaluate(reactive)
    else dependencies.foreach(register(reactive))
    reactive
  }

  def ensureLevel(dependant: Reactive[S], dependencies: Set[Reactive[S]]): Int =
    if (dependencies.isEmpty) 0
    else {
      val newLevel = dependencies.map(_.bud.level).max + 1
      dependant.bud.updateLevel(newLevel)
    }

  // TODO Need to synchronize the queue? I thinkg it is necessary
  def admit(reactive: Reactive[S]): Unit = levelQueue.enqueue(reactive.bud.level)(reactive)
  def forget(reactive: Reactive[S]): Unit = levelQueue.remove(reactive)

  /** allow turn to handle dynamic access to reactives */
  override def dependencyInteraction(dependency: Reactive[S]): Unit = ()

  def preparationPhase(initialWrites: List[Reactive[S]]): Unit = initialWrites.foreach(admit)

  def propagationPhase(): Unit

  def commitPhase() = toCommit.foreach(_.commit(this))

  def rollbackPhase() = toCommit.foreach(_.release(this))

  def observerPhase() = observers.foreach(_.apply())

  def releasePhase(): Unit

}