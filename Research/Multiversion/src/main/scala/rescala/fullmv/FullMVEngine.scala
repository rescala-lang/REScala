package rescala.fullmv

import java.util.concurrent.ForkJoinPool

import rescala.core.{EngineImpl, Pulsing}
import rescala.fullmv.tasks.{Framing, Notification}

import scala.util.Try

object FullMVEngine extends EngineImpl[FullMVStruct, FullMVTurn] {
  val EXECUTE_WRAPUP_SEQUENTIALLY = false

  val DEBUG = false

  val threadPool = new ForkJoinPool()

  val sgt = DecentralizedSGT

  override private[rescala] def singleNow[A](reactive: Pulsing[A, FullMVStruct]) = reactive.state.latestValue

  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[FullMVTurn]): FullMVTurn = new FullMVTurn()
  override protected def executeInternal[I, R](turn: FullMVTurn, initialWrites: Traversable[Reactive], admissionPhase: () => I, wrapUpPhase: I => R): R = {
    if(initialWrites.nonEmpty) {
      // framing phase
      turn.awaitAndSwitchPhase(TurnPhase.Framing)
      turn.activeBranchDifferential(TurnPhase.Framing, initialWrites.size)
      for (i <- initialWrites) threadPool.submit(Framing(turn, i))
    }

    turn.awaitAndSwitchPhase(TurnPhase.Executing)

    // admission phase
    val admissionResult = Try(admissionPhase())
    if(DEBUG) admissionResult match {
      case scala.util.Failure(e) => e.printStackTrace()
      case _ =>
    }
    assert(turn.activeBranches == 0, s"Admission phase left ${turn.activeBranches} active branches.")

    // propagation phase
    turn.activeBranchDifferential(TurnPhase.Executing, initialWrites.size)
    for(i <- initialWrites) threadPool.submit(Notification(turn, i, changed = admissionResult.isSuccess))

    // propagation completion
    if(EXECUTE_WRAPUP_SEQUENTIALLY) turn.awaitAndSwitchPhase(TurnPhase.WrapUp)

    // wrap-up "phase" (executes in parallel with propagation)
    val result = admissionResult.flatMap(i => Try { wrapUpPhase(i) })
    if(EXECUTE_WRAPUP_SEQUENTIALLY) assert(turn.activeBranches == 0, s"WrapUp phase left ${turn.activeBranches} active branches.")

    // turn completion
    turn.awaitAndSwitchPhase(TurnPhase.Completed)

    // result
    result.get
  }

  val CREATE_PRETURN: FullMVTurn = new FullMVTurn()
  CREATE_PRETURN.awaitAndSwitchPhase(TurnPhase.Completed)
}
