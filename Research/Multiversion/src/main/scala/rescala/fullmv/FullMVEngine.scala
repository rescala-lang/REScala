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

  val sgt = DumbSGT


  override private[rescala] def singleNow[A](reactive: Pulsing[A, FullMVStruct]) = reactive.state.latestValue

  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[FullMVTurn]): FullMVTurn = new FullMVTurn(sgt)
  override protected def executeInternal[I, R](turn: FullMVTurn, initialWrites: Traversable[Reactive], admissionPhase: () => I, wrapUpPhase: I => R): R = {
    if(initialWrites.nonEmpty) {
      // framing start
      turn.beginPhase(TurnPhase.Framing, initialWrites.size)
      for (i <- initialWrites) threadPool.submit(Framing(turn, i))

      // framing completion
      turn.awaitBranches()
      sgt.awaitAllPredecessorsState(turn, TurnPhase.Executing)
    }

    // admission
    turn.beginPhase(TurnPhase.Executing, initialWrites.size)
    val admissionResult = Try(admissionPhase())

    // propagation start
    for(i <- initialWrites) threadPool.submit(Notification(turn, i, changed = admissionResult.isSuccess))

    // propagation completion
    @tailrec def awaitPropagationCompletion(completedReevaluationsBefore: Int): Unit = {
      sgt.awaitAllPredecessorsState(turn, TurnPhase.WrapUp)
      val completedReevaluationsAfter = turn.awaitBranches()
      if(completedReevaluationsBefore != completedReevaluationsAfter) {
        // retrofitted reevaluations may perform dynamic reads that establish orders with previously
        // entirely unrelated other turns, hence we have to repeat waiting here.
        awaitPropagationCompletion(completedReevaluationsAfter)
      }
    }
    awaitPropagationCompletion(0)

    // wrap-up
    turn.beginPhase(TurnPhase.WrapUp, 0)
    val result = admissionResult.flatMap(i => Try { wrapUpPhase(i) })

    // turn completion
    sgt.awaitAllPredecessorsState(turn, TurnPhase.Completed)
    turn.beginPhase(TurnPhase.Completed, -1)
    sgt.completed(turn)

    // result
    result.get
  }
}
