package rescala.propagation

import rescala.graph.{Commitable, Reactive}
import rescala.turns.Turn
import rescala.propagation.Evaluator.Result

abstract class AbstractTurn extends Turn {
  implicit def currentTurn: AbstractTurn = this

  protected var toCommit = Set[Commitable]()
  protected var afterCommitHandlers = List[() => Unit]()

  protected var initialSources: List[Reactive] = Nil
  protected var initialValues: List[() => Boolean] = Nil

  def handleDiff(res: Result): Result = {
    res.getDiff.foreach { diff =>
      diff.novel foreach acquireDynamic
      diff.removed foreach unregister(res.head)
      diff.added foreach register(res.head)
    }
    res
  }

  val levelQueue = new LevelQueue(evaluate)

  def evaluate(r: Reactive): Unit = handleDiff(Evaluator.evaluate(r)).requeue(levelQueue.enqueue)

  def acquireDynamic(reactive: Reactive): Unit

  def register(sink: Reactive)(source: Reactive): Unit = {
    source.dependants.transform(_ + sink)
  }

  def unregister(sink: Reactive)(source: Reactive): Unit = {
    acquireDynamic(source)
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
  override def admit(source: Reactive)(setPulse: => Boolean): Unit = {
    initialSources ::= source
    initialValues ::= setPulse _
  }

  def lockingPhase(): Unit

  def propagationPhase(): Unit = {
    initialSources.foreach(levelQueue.enqueue(0))
    levelQueue.evaluateQueue()
  }

  def commitPhase() = toCommit.foreach(_.commit(this))

  def observerPhase() = afterCommitHandlers.foreach(_())

  def realeasePhase(): Unit

}