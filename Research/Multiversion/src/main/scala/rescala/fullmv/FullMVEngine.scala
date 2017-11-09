package rescala.fullmv

import java.util.concurrent.{Executor, ForkJoinPool}

import rescala.core.{EngineImpl, ReSourciV}
import rescala.fullmv.NotificationResultAction.GlitchFreeReady
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.{FollowFraming, NoSuccessor}
import rescala.fullmv.mirrors.{FullMVTurnHost, Host, HostImpl, SubsumableLockHostImpl}
import rescala.fullmv.tasks.{Framing, Notification, NotificationWithFollowFrame}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.concurrent.duration._
import scala.util.Try

class FullMVEngine(val timeout: Duration, val name: String) extends EngineImpl[FullMVStruct, FullMVTurn] with FullMVTurnHost with HostImpl[FullMVTurn] {
  override object lockHost extends SubsumableLockHostImpl {
    override def toString: String = "Locks " + name
  }
  def newTurn(): FullMVTurnImpl = createLocal(new FullMVTurnImpl(this, _, Thread.currentThread(), lockHost.newLock()))
  override val dummy: FullMVTurnImpl = {
    val dummy = new FullMVTurnImpl(this, Host.dummyGuid, null, lockHost.newLock())
    instances.put(Host.dummyGuid, dummy)
    dummy.awaitAndSwitchPhase(TurnPhase.Completed)
    dummy
  }

  val threadPool = new ForkJoinPool()

  override private[rescala] def singleNow[A](reactive: ReSourciV[A, FullMVStruct]) = reactive.state.latestValue

  override private[rescala] def executeTurn[R](declaredWrites: Traversable[ReSource], admissionPhase: (AdmissionTicket) => R): R = {
    val turn = newTurn()
    val setWrites = declaredWrites.toSet // this *should* be part of the interface..
    if(setWrites.nonEmpty) {
      // framing phase
      turn.awaitAndSwitchPhase(TurnPhase.Framing)
      turn.activeBranchDifferential(TurnPhase.Framing, setWrites.size)
      for (i <- setWrites) threadPool.submit(Framing(turn, i))
    }

    turn.awaitAndSwitchPhase(TurnPhase.Executing)

    // admission phase
    val admissionTicket = turn.makeAdmissionPhaseTicket()
    val admissionResult = Try { withTurn(turn) { admissionPhase(admissionTicket) } }
    if(FullMVEngine.DEBUG) admissionResult match {
      case scala.util.Failure(e) => e.printStackTrace()
      case _ =>
    }
    assert(turn.activeBranches.get == 0, s"Admission phase left ${turn.activeBranches.get} active branches.")

    // propagation phase
    if(setWrites.nonEmpty) {
      turn.activeBranchDifferential(TurnPhase.Executing, setWrites.size)
      val noChanges = if (admissionResult.isFailure) {
        setWrites
      } else {
        for (change <- admissionTicket.initialChanges) {
          val res = change.v(
            if (change.r.state.asInstanceOf[NodeVersionHistory[_, _, _, _]].valuePersistency.isTransient) {
              change.r.state.asInstanceOf[NodeVersionHistory[change.r.Value, _, _, _]].valuePersistency.initialValue
            } else {
              change.r.state.dynamicBefore(turn)
            })
          val notificationResult = change.r.state.notify(turn, changed = true)
          assert(notificationResult == GlitchFreeReady)
          val reevOutResult = change.r.state.reevOut(turn, if (res.valueChanged) Some(res.value) else None)
          reevOutResult match {
            case NoSuccessor(out) =>
              val diff = out.size - 1
              if (diff != 0) turn.activeBranchDifferential(TurnPhase.Executing, diff)
              for (succ <- out) threadPool.submit(Notification(turn, succ, res.valueChanged))
            case FollowFraming(out, succTxn: FullMVTurn) =>
              val diff = out.size - 1
              if (diff != 0) turn.activeBranchDifferential(TurnPhase.Executing, diff)
              for (succ <- out) threadPool.submit(NotificationWithFollowFrame(turn, succ, res.valueChanged, succTxn))
            case otherwise => throw new AssertionError("Source reevaluation should not be able to yield " + otherwise)
          }
        }
        setWrites.diff(admissionTicket.initialChanges.map(_.r).toSet)
      }
      for (i <- noChanges) {
        val notificationResult = i.state.notify(turn, changed = false)
        notificationResult match {
          case NoSuccessor(out) =>
            val diff = out.size - 1
            if (diff != 0) turn.activeBranchDifferential(TurnPhase.Executing, diff)
            for (succ <- out) threadPool.submit(Notification(turn, succ, changed = false))
          case FollowFraming(out, succTxn: FullMVTurn) =>
            val diff = out.size - 1
            if (diff != 0) turn.activeBranchDifferential(TurnPhase.Executing, diff)
            for (succ <- out) threadPool.submit(NotificationWithFollowFrame(turn, succ, changed = false, succTxn))
          case otherwise => throw new AssertionError("Source reevaluation should not be able to yield " + otherwise)
        }
      }
    }

    // propagation completion
    if(FullMVEngine.SEPARATE_WRAPUP_PHASE) turn.awaitAndSwitchPhase(TurnPhase.WrapUp)

    // wrap-up "phase" (executes in parallel with propagation)
    admissionResult.map{ i => admissionTicket.wrapUp(turn.makeWrapUpPhaseTicket()); i }

    if(FullMVEngine.SEPARATE_WRAPUP_PHASE) assert(turn.activeBranches.get == 0, s"WrapUp phase left ${turn.activeBranches.get} active branches.")

    // turn completion
    turn.awaitAndSwitchPhase(TurnPhase.Completed)

    // result
    admissionResult.get
  }

  override def toString: String = "Host " + name
}

object FullMVEngine {
  val SEPARATE_WRAPUP_PHASE = false
  val DEBUG = false

  val default = new FullMVEngine(10.seconds, "default")

  val notWorthToMoveToTaskpool: ExecutionContextExecutor = ExecutionContext.fromExecutor(new Executor{
    override def execute(command: Runnable): Unit = command.run()
  })
}
