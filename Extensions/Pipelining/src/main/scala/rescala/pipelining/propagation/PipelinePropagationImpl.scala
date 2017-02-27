package rescala.pipelining.propagation

import rescala.graph.{Reactive, ReevaluationResult}
import rescala.pipelining.PipelineStruct
import rescala.propagation.Turn
import rescala.twoversion.{AbstractPropagation, Committable}


private[pipelining] trait PipelinePropagationImpl extends AbstractPropagation[PipelineStruct.type] {

  type S = PipelineStruct.type

  implicit def currentTurn: PipelinePropagationImpl = this

  protected[this] var toCommit = Set[Committable]()
  private var observers = List.empty[() => Unit]

  val levelQueue = new PipelineQueue()

  protected def requeue(head: Reactive[S], changed: Boolean, level: Int, action: QueueAction): Unit = action match {
    case EnqueueDependencies =>  head.state.outgoing.foreach(levelQueue.enqueue(level, changed))
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

  def maximumLevel(dependencies: Set[Reactive[S]])(implicit turn: Turn[S]): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.state.level))

  def register(sink: Reactive[S])(source: Reactive[S]): Unit = source.state.discover(sink)

  def unregister(sink: Reactive[S])(source: Reactive[S]): Unit = source.state.drop(sink)

  override def schedule(commitable: Committable): Unit = toCommit += commitable

  override def observe(f: () => Unit): Unit = observers ::= f

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
      val newLevel = dependencies.map(_.state.level).max + 1
      dependant.state.updateLevel(newLevel)
    }

  // TODO Need to synchronize the queue? I thinkg it is necessary
  def admit(reactive: Reactive[S]): Unit = levelQueue.enqueue(reactive.state.level)(reactive)
  def forget(reactive: Reactive[S]): Unit = levelQueue.remove(reactive)

  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: Reactive[S]): Unit = ()

  def preparationPhase(initialWrites: Traversable[Reactive[S]]): Unit = initialWrites.foreach(admit)

  def propagationPhase(): Unit

  def commitPhase() = toCommit.foreach(_.commit(this))

  def rollbackPhase() = toCommit.foreach(_.release(this))

  def observerPhase() = observers.foreach(_.apply())

  def releasePhase(): Unit

}
