package rescala.fullmv

import java.util.concurrent.{Executor, ForkJoinPool}

import rescala.core.{EngineImpl, ReSourciV}
import rescala.fullmv.mirrors.{FullMVTurnHost, Host, HostImpl, SubsumableLockHostImpl}
import rescala.fullmv.tasks.{Framing, SourceNotification}

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
    withTurn(turn) {
      val setWrites = declaredWrites.toSet // this *should* be part of the interface..
      if (setWrites.nonEmpty) {
        // framing phase
        turn.beginFraming()
        turn.activeBranchDifferential(TurnPhase.Framing, setWrites.size)
        for (i <- setWrites) threadPool.submit(Framing(turn, i))
        turn.completeFraming()
      } else {
        turn.beginExecuting()
      }

      // admission phase
      val admissionTicket = turn.makeAdmissionPhaseTicket()
      val admissionResult = Try { admissionPhase(admissionTicket) }
      if (FullMVEngine.DEBUG) admissionResult match {
        case scala.util.Failure(e) => e.printStackTrace()
        case _ =>
      }
      assert(turn.activeBranches.get == 0, s"Admission phase left ${turn.activeBranches.get()} tasks undone.")

      // propagation phase
      if (setWrites.nonEmpty) {
        turn.initialChanges = admissionTicket.initialChanges
        turn.activeBranchDifferential(TurnPhase.Executing, setWrites.size)
        for(write <- setWrites) threadPool.submit(SourceNotification(turn, write, admissionResult.isSuccess && admissionTicket.initialChanges.contains(write)))
      }

      // wrap-up "phase" (executes in parallel with propagation)
      val transactionResult = if(admissionTicket.wrapUp == null){
        admissionResult
      } else {
        val wrapUpTicket = turn.makeWrapUpPhaseTicket()
        admissionResult.map{ i =>
          // executed in map call so that exceptions in wrapUp make the transaction result a Failure
          admissionTicket.wrapUp(wrapUpTicket)
          i
        }
      }

      // turn completion
      turn.completeExecuting()

      // result
      transactionResult.get
    }
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
