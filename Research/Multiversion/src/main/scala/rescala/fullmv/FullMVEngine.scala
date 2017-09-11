package rescala.fullmv

import java.util.concurrent.ForkJoinPool

import rescala.core.{EngineImpl, ReSourciV, ReactiV}
import rescala.fullmv.mirrors.{FullMVTurnHost, Host, HostImpl, SubsumableLockHostImpl}
import rescala.fullmv.tasks.{Framing, Notification, Reevaluation}

import scala.concurrent.duration._
import scala.util.Try

class FullMVEngine(val timeout: Duration, val name: String) extends EngineImpl[FullMVStruct, FullMVTurn] with FullMVTurnHost with HostImpl[FullMVTurn] {
  override object lockHost extends SubsumableLockHostImpl
  def newTurn(): FullMVTurnImpl = createLocal(new FullMVTurnImpl(this, _, Thread.currentThread(), timeout, lockHost.newLock()))
  override val dummy: FullMVTurnImpl = {
    val dummy = new FullMVTurnImpl(this, Host.dummyGuid, null, Duration.Zero, null)
    instances.put(Host.dummyGuid, dummy)
    dummy.awaitAndSwitchPhase(TurnPhase.Completed)
    dummy
  }

  val threadPool = new ForkJoinPool()

  override private[rescala] def singleNow[A](reactive: ReSourciV[A, FullMVStruct]) = reactive.state.latestValue


  override private[rescala] def executeTurn[R](declaredWrites: Traversable[Reactive], admissionPhase: (AdmissionTicket) => R): R = {
    val turn = newTurn()

    if(declaredWrites.nonEmpty) {
      // framing phase
      turn.awaitAndSwitchPhase(TurnPhase.Framing)
      turn.activeBranchDifferential(TurnPhase.Framing, declaredWrites.size)
      for (i <- declaredWrites) threadPool.submit(Framing(turn, i))
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
    if(declaredWrites.nonEmpty){
      turn.activeBranchDifferential(TurnPhase.Executing, declaredWrites.size)

      val noChanges = admissionResult match {
        case scala.util.Failure(_) => declaredWrites
        case _ =>
          for (change <- admissionTicket.initialChanges) {
            val res = change.v(change.r.state.reevIn(turn))
            res.commitDependencyDiff(turn, change.r)
            change.r.state.notify(turn, changed = true)
            val changePropagationTasks = Reevaluation.processReevaluationResult(change.r, turn, change.r.state.reevOut(turn, if (res.valueChanged) Some(res.value) else None), res.valueChanged)
            for(task <- changePropagationTasks) threadPool.submit(task)
          }
          declaredWrites.toSet.diff(admissionTicket.initialChanges.map(_.r).toSet)
      }
      for(i <- noChanges) threadPool.submit(Notification(turn, i, changed = false))
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
}
