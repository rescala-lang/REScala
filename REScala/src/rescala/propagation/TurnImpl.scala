package rescala.propagation

import rescala.graph.{Commitable, Reactive}
import rescala.turns.Turn
import rescala.propagation.Evaluator.Result

class TurnImpl extends Turn {
  implicit def currentTurn: TurnImpl = this

  protected var toCommit = Set[Commitable]()
  protected var afterCommitHandlers = List[() => Unit]()

  protected var initialReactives: List[Reactive] = Nil
  protected var initialClosures: List[() => Unit] = Nil

  def handleDiff(res: Result): Result = {
    res.getDiff.foreach { diff =>
      diff.removed foreach unregister(res.head)
      diff.added foreach register(res.head)
    }
    res
  }

  val levelQueue = new LevelQueue(evaluate)

  def evaluate(r: Reactive): Unit = handleDiff(Evaluator.evaluate(r)).requeue(levelQueue.enqueue)

  def register(sink: Reactive)(source: Reactive): Unit = {
    source.dependants.transform(_ + sink)
  }

  def unregister(sink: Reactive)(source: Reactive): Unit = {
    source.dependants.transform(_ - sink)
  }

  def plan(commitable: Commitable): Unit = {
    toCommit += commitable
  }

  override def afterCommit(handler: => Unit) = afterCommitHandlers ::= handler _

  def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    val reactive = f
    dependencies.foreach(register(reactive))
    ensureLevel(reactive, dependencies)
    reactive
  }

  def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    val reactive = f
    ensureLevel(reactive, dependencies)
    evaluate(reactive)
    reactive
  }

  def ensureLevel(dependant: Reactive, dependencies: Set[Reactive]): Int =
    if (dependencies.isEmpty) 0
    else {
      val newLevel = dependencies.map(_.level.get).max + 1
      dependant.level.transform(math.max(newLevel, _))
    }

  /** admits a new source change */
  override def admit(writes: Reactive*)(f: => Unit): Unit = {
    initialReactives :::= writes.toList
    initialClosures ::= f _
  }

  def lockPhase(): Unit = ()

  def admissionPhase(): Unit = initialClosures.foreach(_())

  def propagationPhase(): Unit = {
    initialReactives.foreach(levelQueue.enqueue(0))
    levelQueue.evaluateQueue()
  }

  def commitPhase() = toCommit.foreach(_.commit(this))

  def rollbackPhase() = toCommit.foreach(_.release(this))

  def observerPhase() = afterCommitHandlers.foreach(_())

  def realeasePhase(): Unit = ()

}