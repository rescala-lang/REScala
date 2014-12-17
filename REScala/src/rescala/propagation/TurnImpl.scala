package rescala.propagation

import rescala.graph.{Commitable, Reactive}
import rescala.propagation.Evaluator.Result
import rescala.turns.{Engine, Turn}

class TurnImpl(override val engine: Engine[Turn]) extends Turn {
  implicit def currentTurn: TurnImpl = this

  protected var toCommit = Set[Commitable]()
  protected var afterCommitHandlers = List[() => Unit]()

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

  override def plan(commitable: Commitable): Unit = {
    toCommit += commitable
  }

  override def afterCommit(handler: => Unit) = afterCommitHandlers ::= handler _

  override def create[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
    val reactive = f
    dependencies.foreach(register(reactive))
    ensureLevel(reactive, dependencies)
    reactive
  }

  override def createDynamic[T <: Reactive](dependencies: Set[Reactive])(f: => T): T = {
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

  override def admit(reactive: Reactive): Unit = levelQueue.enqueue(reactive.level.get)(reactive)
  override def forget(reactive: Reactive): Unit = levelQueue.remove(reactive)

  /** allow turn to handle dynamic access to reactives */
  override def accessDynamic(dependency: Reactive): Unit = ()

  def lockPhase(initialWrites: List[Reactive]): Unit = ()

  def propagationPhase(): Unit = levelQueue.evaluateQueue()

  def commitPhase() = toCommit.foreach(_.commit(this))

  def rollbackPhase() = toCommit.foreach(_.release(this))

  def observerPhase() = afterCommitHandlers.foreach(_())

  def realeasePhase(): Unit = ()

}