package rescala.fullmv.tasks

import rescala.core.{AccessHandler, Derived, ReSource, ReevTicket, Result}
import rescala.fullmv.FramingBranchResult._
import rescala.fullmv.NotificationBranchResult.ReevOutBranchResult.{
  NotifyAndNonReadySuccessor, NotifyAndReevaluationReadySuccessor, PureNotifyOnly
}
import rescala.fullmv.NotificationBranchResult.{ReevOutBranchResult, _}
import rescala.fullmv.mirrors.Mirror
import rescala.fullmv.sgt.synchronization.SubsumableLockBundle
import rescala.fullmv.{FullMVBundle, _}
import rescala.operator.Pulse

import java.util.concurrent.RecursiveAction

trait TaskBundle extends FullMVBundle {
  selfType: Mirror with TurnImplBundle with FullMvStateBundle with SubsumableLockBundle =>

  trait FramingTask extends FullMVAction {
    override def doCompute(): Unit = {
      val branchResult = doFraming()
      if (FullMVUtil.DEBUG) println(s"[${Thread.currentThread().getName}] $this => $branchResult")
      branchResult match {
        case FramingBranchEnd =>
          turn.activeBranchDifferential(TurnPhase.Framing, -1)
        case Frame(out: Set[ReSource.of[State]], maybeOtherTurn) =>
          branchCountDiffOnBranchOut(out, maybeOtherTurn)
          for (dep <- out) new Framing(maybeOtherTurn, dep).fork
        case FrameSupersede(out: Set[ReSource.of[State]], maybeOtherTurn, supersede) =>
          branchCountDiffOnBranchOut(out, maybeOtherTurn)
          for (dep <- out) new SupersedeFraming(maybeOtherTurn, dep, supersede).fork
      }
    }

    private def branchCountDiffOnBranchOut(out: Set[ReSource.of[State]], maybeOtherTurn: FullMVTurn): Unit = {
      if (turn == maybeOtherTurn) {
        val branchDiff = out.size - 1
        if (branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Framing, branchDiff)
      } else {
        if (out.nonEmpty) maybeOtherTurn.activeBranchDifferential(TurnPhase.Framing, out.size)
        turn.activeBranchDifferential(TurnPhase.Framing, -1)
      }
    }

    def doFraming(): FramingBranchResult[FullMVTurn, ReSource.of[State]]
  }

  class Framing(override val turn: FullMVTurn, override val node: ReSource.of[State]) extends FramingTask {
    override def doFraming(): FramingBranchResult[FullMVTurn, ReSource.of[State]] = {
      assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
      node.state.incrementFrame(turn)
    }
    override def toString = s"Framing($turn, $node)"
  }

  class SupersedeFraming(override val turn: FullMVTurn, override val node: ReSource.of[State], supersede: FullMVTurn)
      extends FramingTask {
    override def doFraming(): FramingBranchResult[FullMVTurn, ReSource.of[State]] = {
      assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
      assert(supersede.phase == TurnPhase.Framing, s"$supersede cannot have frame superseded (requires framing phase)")
      node.state.incrementSupersedeFrame(turn, supersede)
    }
    override def toString = s"Framing($turn, $node)"
  }

  trait FullMVAction extends RecursiveAction {
    val turn: FullMVTurn
    val node: ReSource.of[State]
    override def compute(): Unit = {
      try { doCompute() }
      catch {
        case t: Throwable =>
          new Exception(this.toString + " failed on " + Thread.currentThread().getName, t).printStackTrace()
      }
    }
    def doCompute(): Unit
  }

  trait NotificationAction[N <: ReSource.of[State]] extends ReevaluationHandling[N] {
    val changed: Boolean
    override def doCompute(): Unit = {
      val (retainBranch, notificationResultAction) = deliverNotification()
      if (FullMVUtil.DEBUG)
        println(s"[${Thread.currentThread().getName}] $this => $retainBranch, $notificationResultAction")
      processNotificationResult(retainBranch, notificationResultAction)
    }

    def processNotificationResult(
        retainBranch: Boolean,
        notificationResultAction: NotificationBranchResult[FullMVTurn, Derived.of[State]]
    ): Unit = {
      notificationResultAction match {
        case DoNothing =>
          if (!retainBranch) turn.activeBranchDifferential(TurnPhase.Executing, -1)
        case ReevaluationReady =>
          doReevaluation(retainBranch)
        case outAndSucc: ReevOutBranchResult[FullMVTurn @unchecked, Derived.of[State] @unchecked] =>
          processReevOutResult(retainBranch, outAndSucc, changed = false)
      }
    }

    def deliverNotification(): (Boolean, NotificationBranchResult[FullMVTurn, Derived.of[State]])
  }

  class SourceNotification(
      override val turn: FullMVTurn,
      override val node: ReSource.of[State],
      override val changed: Boolean
  ) extends NotificationAction[ReSource.of[State]]
      with SourceReevaluationHandling {
    override def deliverNotification(): (Boolean, NotificationBranchResult[FullMVTurn, Derived.of[State]]) =
      node.state.notify(turn, changed)
    override def createReevaluation(succTxn: FullMVTurn) = new SourceReevaluation(succTxn, node)
    override def toString                                = s"SourceNotification($turn, $node)"
  }

  class Notification(
      override val turn: FullMVTurn,
      override val node: Derived.of[State],
      override val changed: Boolean
  ) extends NotificationAction[Derived.of[State]]
      with RegularReevaluationHandling {
    override def deliverNotification(): (Boolean, NotificationBranchResult[FullMVTurn, Derived.of[State]]) =
      node.state.notify(turn, changed)
    override def createReevaluation(succTxn: FullMVTurn) = new Reevaluation(succTxn, node)
    override def toString                                = s"Notification($turn, $node)"
  }
  class NotificationWithFollowFrame(
      override val turn: FullMVTurn,
      override val node: Derived.of[State],
      override val changed: Boolean,
      followFrame: FullMVTurn
  ) extends NotificationAction[Derived.of[State]]
      with RegularReevaluationHandling {
    override def deliverNotification(): (Boolean, NotificationBranchResult[FullMVTurn, Derived.of[State]]) =
      node.state.notifyFollowFrame(turn, changed, followFrame)
    override def createReevaluation(succTxn: FullMVTurn) = new Reevaluation(succTxn, node)
    override def toString                                = s"NotificationWithFollowFrame($turn, $node, $followFrame)"
  }

  class Reevaluation(override val turn: FullMVTurn, override val node: Derived.of[State])
      extends RegularReevaluationHandling {
    override def doCompute(): Unit = doReevaluation(retainBranch = true)
    override def toString          = s"Reevaluation($turn, $node)"
  }

  trait RegularReevaluationHandling extends ReevaluationHandling[Derived.of[State]] {
    override val node: Derived.of[State]

    object FullAccessHandle extends AccessHandler[State] {
      override def staticAccess(reactive: ReSource.of[State]): reactive.Value  = turn.staticAfter(reactive)
      override def dynamicAccess(reactive: ReSource.of[State]): reactive.Value = turn.dynamicAfter(reactive)
    }

    def doReevaluation(retainBranch: Boolean): Unit = {
//    assert(Thread.currentThread() == turn.userlandThread, s"$this on different thread ${Thread.currentThread().getName}")
      assert(turn.phase == TurnPhase.Executing, s"$turn cannot reevaluate (requires executing phase")
      var value             = node.state.reevIn(turn)
      val transactionHandle = TransactionHandle(turn)
      val ticket: ReevTicket[State, node.Value] =
        new ReevTicket[State, node.Value](transactionHandle, value, FullAccessHandle)
      val res: Result.of[State, node.Value] =
        try {
          turn.host.withDynamicInitializer(transactionHandle) {
            node.reevaluate(ticket)
          }
        } catch {
          case exception: Throwable =>
            System.err.println(
              s"[FullMV Error] Reevaluation of $node failed with ${exception.getClass.getName}: ${exception.getMessage}; Completing reevaluation as NoChange."
            )
            exception.printStackTrace()
            ticket.withPropagate(false)
        }
      res.inputs().foreach(commitDependencyDiff(node, node.state.incomings))
      res.forValue(v => value = v)
      res.forEffect(_.execute())
      val res2 = processReevaluationResult(if (res.activate) Some(value) else None)
      processReevOutResult(retainBranch, res2, changed = res.activate)
    }

    final def commitDependencyDiff(
        node: Derived.of[State],
        current: Set[ReSource.of[State]]
    )(updated: Set[ReSource.of[State]]): Unit = {
      val indepsRemoved = current -- updated
      val indepsAdded   = updated -- current
      indepsRemoved.foreach(turn.drop(_, node))
      indepsAdded.foreach(turn.discover(_, node))
      turn.writeIndeps(node, updated)
    }

    override def createReevaluation(succTxn: FullMVTurn) = new Reevaluation(succTxn, node)
  }

  class SourceReevaluation(override val turn: FullMVTurn, override val node: ReSource.of[State])
      extends SourceReevaluationHandling {
    override def doCompute(): Unit = doReevaluation(retainBranch = true)
    override def toString          = s"SourceReevaluation($turn, $node)"
  }

  trait SourceReevaluationHandling extends ReevaluationHandling[ReSource.of[State]] {
    def doReevaluation(retainBranch: Boolean): Unit = {
//    assert(Thread.currentThread() == turn.userlandThread, s"$this on different thread ${Thread.currentThread().getName}")
      assert(turn.phase == TurnPhase.Executing, s"$turn cannot source-reevaluate (requires executing phase")
      val ic = turn.asInstanceOf[FullMVTurnImpl].initialChanges(node)
      assert(ic.source eq node, s"$turn initial change map broken?")
      if (
        !ic.writeValue(
          ic.source.state.latestValue,
          x => {
            val res = processReevaluationResult(Some(x.asInstanceOf[node.Value]))
            processReevOutResult(retainBranch, res, changed = true)
          }
        )
      ) {
        val res = processReevaluationResult(None)
        processReevOutResult(retainBranch, res, changed = false)
      }
    }

    override def createReevaluation(succTxn: FullMVTurn): FullMVAction = new SourceReevaluation(succTxn, node)
  }

  trait ReevaluationHandling[N <: ReSource.of[State]] extends FullMVAction {
    def createReevaluation(succTxn: FullMVTurn): FullMVAction
    def doReevaluation(retainBranch: Boolean): Unit

    def processReevaluationResult(maybeChange: Option[node.Value])
        : ReevOutBranchResult[FullMVTurn, Derived.of[State]] = {
      val reevOutResult = node.state.reevOut(turn, maybeChange, node.commit)
      if (FullMVUtil.DEBUG && maybeChange.isDefined && maybeChange.get.isInstanceOf[Pulse.Exceptional]) {
        // could be a framework exception that is relevant to debugging, but was eaten by reactive's
        // exception propagation and thus wouldn't be reported otherwise..
        println(s"[${Thread.currentThread().getName}] WARNING: $this glitch-free result is exceptional:")
        maybeChange.get.asInstanceOf[Pulse.Exceptional].throwable.printStackTrace()
      }
      if (FullMVUtil.DEBUG)
        println(
          s"[${Thread.currentThread().getName}] Reevaluation($turn,$node) => ${
              if (maybeChange.isDefined) "changed"
              else "unchanged"
            } $reevOutResult"
        )
      reevOutResult
    }

    def processReevOutResult(
        retainBranch: Boolean,
        outAndSucc: ReevOutBranchResult[FullMVTurn, Derived.of[State]],
        changed: Boolean
    ): Unit = {
      (outAndSucc: @unchecked) match {
        case PureNotifyOnly(out) =>
          doBranchDiff(retainBranch, out)
          for (dep <- out) new Notification(turn, dep, changed).fork()
        case NotifyAndNonReadySuccessor(out, succTxn) =>
          doBranchDiff(retainBranch, out)
          for (dep <- out) new NotificationWithFollowFrame(turn, dep, changed, succTxn).fork()
        case NotifyAndReevaluationReadySuccessor(out, succTxn) =>
          doBranchDiff(retainBranch, out)
          for (dep <- out) new NotificationWithFollowFrame(turn, dep, changed, succTxn).fork()
          createReevaluation(succTxn).fork()
          ()
      }
    }

    private def doBranchDiff(retainBranch: Boolean, out: Set[Derived.of[State]]) = {
      val branchDiff = out.size - (if (retainBranch) 1 else 2)
      if (branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Executing, branchDiff)
    }
  }

}
