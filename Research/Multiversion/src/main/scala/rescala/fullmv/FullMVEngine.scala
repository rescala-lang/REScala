package rescala.fullmv

import java.util.concurrent.ForkJoinPool

import rescala.core.{EngineImpl, ReadableReactive}
import rescala.fullmv.mirrors.{FullMVTurnHost, Host, HostImpl, SubsumableLockHostImpl}
import rescala.fullmv.tasks.{Framing, Notification}

import scala.concurrent.duration.Duration
import scala.util.Try

class FullMVEngine(val timeout: Duration, val name: String) extends EngineImpl[FullMVStruct, FullMVTurn, FullMVTurnImpl] with FullMVTurnHost with HostImpl[FullMVTurn] {
  override object lockHost extends SubsumableLockHostImpl
  def newTurn(): FullMVTurnImpl = createLocal(new FullMVTurnImpl(this, _, Thread.currentThread(), timeout, lockHost.newLock()))
  override val dummy: FullMVTurnImpl = {
    val dummy = new FullMVTurnImpl(this, Host.dummyGuid, null, Duration.Zero, null)
    instances.put(Host.dummyGuid, dummy)
    dummy.awaitAndSwitchPhase(TurnPhase.Completed)
    dummy
  }

  val threadPool = new ForkJoinPool()

  override private[rescala] def singleNow[A](reactive: ReadableReactive[A, FullMVStruct]) = reactive.state.latestValue

  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[FullMVTurn]): FullMVTurnImpl = newTurn()
  override protected def executeInternal[I, R](turn: FullMVTurnImpl, initialWrites: Traversable[Reactive], admissionPhase: () => I, wrapUpPhase: I => R): R = {
    if(initialWrites.nonEmpty) {
      // framing phase
      turn.awaitAndSwitchPhase(TurnPhase.Framing)
      turn.activeBranchDifferential(TurnPhase.Framing, initialWrites.size)
      for (i <- initialWrites) threadPool.submit(Framing(turn, i))
    }

    turn.awaitAndSwitchPhase(TurnPhase.Executing)

    // admission phase
    val admissionResult = Try(admissionPhase())
    if(FullMVEngine.DEBUG) admissionResult match {
      case scala.util.Failure(e) => e.printStackTrace()
      case _ =>
    }
    assert(turn.activeBranches.get == 0, s"Admission phase left ${turn.activeBranches.get} active branches.")

    // propagation phase
    if(initialWrites.nonEmpty){
      turn.activeBranchDifferential(TurnPhase.Executing, initialWrites.size)
      for(i <- initialWrites) threadPool.submit(Notification(turn, i, changed = admissionResult.isSuccess))
    }

    // propagation completion
    if(FullMVEngine.SEPARATE_WRAPUP_PHASE) turn.awaitAndSwitchPhase(TurnPhase.WrapUp)

    // wrap-up "phase" (executes in parallel with propagation)
    val result = admissionResult.flatMap(i => Try { wrapUpPhase(i) })
    if(FullMVEngine.SEPARATE_WRAPUP_PHASE) assert(turn.activeBranches.get == 0, s"WrapUp phase left ${turn.activeBranches.get} active branches.")

    // turn completion
    turn.awaitAndSwitchPhase(TurnPhase.Completed)

    // result
    result.get
  }

  override def toString: String = "Host " + name
}

object FullMVEngine {
  val SEPARATE_WRAPUP_PHASE = false
  val DEBUG = false

  val default = new FullMVEngine(Duration.Zero, "default")
}
