package rescala.fullmv

import java.util.concurrent.ForkJoinPool

import rescala.engine.EngineImpl
import rescala.fullmv.tasks.{Framing, Notification}
import rescala.graph.Pulsing

import scala.annotation.tailrec
import scala.util.Try

object FullMVEngine extends EngineImpl[FullMVStruct, FullMVTurn] {
  val DEBUG = false

  val threadPool = new ForkJoinPool()

  val sgt = DecentralizedSGT


  override private[rescala] def singleNow[A](reactive: Pulsing[A, FullMVStruct]) = reactive.state.latestValue

  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[FullMVTurn]): FullMVTurn = new FullMVTurn()
  override protected def executeInternal[I, R](turn: FullMVTurn, initialWrites: Traversable[Reactive], admissionPhase: () => I, wrapUpPhase: I => R): R = {
    if(initialWrites.nonEmpty) {
      // framing start
      turn.switchPhase(TurnPhase.Framing)
      turn.activeBranchDifferential(TurnPhase.Framing, initialWrites.size)
      for (i <- initialWrites) threadPool.submit(Framing(turn, i))

      // framing completion
      turn.awaitBranchCountZero()
      turn.awaitAllPredecessorsPhase(TurnPhase.Executing)
    }

    // admission
    turn.switchPhase(TurnPhase.Executing)
    val admissionResult = Try(admissionPhase())
    if(DEBUG) admissionResult match {
      case scala.util.Failure(e) => e.printStackTrace()
      case _ =>
    }
    assert(turn.activeBranches == 0, s"Admission phase left ${turn.activeBranches} active branches.")

    // propagation start
    turn.activeBranchDifferential(TurnPhase.Executing, initialWrites.size)
    for(i <- initialWrites) threadPool.submit(Notification(turn, i, changed = admissionResult.isSuccess))

    // propagation completion
    @tailrec def awaitPropagationCompletion(completedReevaluationsBefore: Int): Unit = {
      turn.awaitAllPredecessorsPhase(TurnPhase.WrapUp)
      val completedReevaluationsAfter = turn.awaitBranchCountZero()
      if(completedReevaluationsBefore != completedReevaluationsAfter) {
        // retrofitted reevaluations may perform dynamic reads that establish orders with previously
        // entirely unrelated other turns, hence we have to repeat waiting here.
        awaitPropagationCompletion(completedReevaluationsAfter)
      }
    }
    awaitPropagationCompletion(turn.awaitBranchCountZero())

    // wrap-up
    turn.switchPhase(TurnPhase.WrapUp)
    val result = admissionResult.flatMap(i => Try { wrapUpPhase(i) })
    assert(turn.activeBranches == 0, s"WrapUp phase left ${turn.activeBranches} active branches.")

    // turn completion
    turn.awaitAllPredecessorsPhase(TurnPhase.Completed)
    turn.switchPhase(TurnPhase.Completed)
    turn.sgtNode.discard()

    // result
    result.get
  }

  val CREATE_PRETURN: FullMVTurn = {
    val preTurn = new FullMVTurn()
    preTurn.switchPhase(TurnPhase.Completed)
    preTurn
  }
}
