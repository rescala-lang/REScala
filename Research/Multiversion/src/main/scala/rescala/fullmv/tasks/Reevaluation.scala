package rescala.fullmv.tasks

import rescala.core._
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.{FollowFraming, NextReevaluation, NoSuccessor}
import rescala.fullmv.NotificationResultAction.{Glitched, ReevOutResult}
import rescala.fullmv._

class Reevaluation(override val turn: FullMVTurn, override val node: Reactive[FullMVStruct]) extends RegularReevaluationHandling {
  override def doCompute(): Unit = doReevaluation(retainBranch = true)
  override def toString = s"Reevaluation($turn, $node)"
}

trait RegularReevaluationHandling extends ReevaluationHandling[Reactive[FullMVStruct]] {
  override val node: Reactive[FullMVStruct]
  def doReevaluation(retainBranch: Boolean): Unit = {
//    assert(Thread.currentThread() == turn.userlandThread, s"$this on different thread ${Thread.currentThread().getName}")
    assert(turn.phase == TurnPhase.Executing, s"$turn cannot reevaluate (requires executing phase")
    var value = node.state.reevIn(turn)
    val ticket: ReevTicket[node.Value, FullMVStruct] = new ReevTicket(turn, value) {
      override protected def staticAccess(reactive: ReSource[FullMVStruct]): reactive.Value = turn.staticAfter(reactive)
      override protected def dynamicAccess(reactive: ReSource[FullMVStruct]): reactive.Value = turn.dynamicAfter(reactive)
    }
    val res: Result[node.Value, FullMVStruct] = try {
      turn.host.withDynamicInitializer(turn) {
        node.reevaluate(ticket)
      }
    } catch {
      case exception: Throwable =>
        System.err.println(s"[FullMV Error] Reevaluation of $node failed with ${exception.getClass.getName}: ${exception.getMessage}; Completing reevaluation as NoChange.")
        exception.printStackTrace()
        ticket.withPropagate(false)
    }
    res.getDependencies().foreach(commitDependencyDiff(node, node.state.incomings))
    res.forValue(v => value = v)
    res.forEffect(_())
    val res2 = processReevaluationResult(if(res.propagate) Some(value) else None)
    processReevOutResult(retainBranch, res2, changed = res.propagate)
  }

  final def commitDependencyDiff(node: Reactive[FullMVStruct], current: Set[ReSource[FullMVStruct]])(updated: Set[ReSource[FullMVStruct]]): Unit = {
    val indepsRemoved = current -- updated
    val indepsAdded = updated -- current
    indepsRemoved.foreach(turn.drop(_, node))
    indepsAdded.foreach(turn.discover(_, node))
    turn.writeIndeps(node, updated)
  }

  override def createReevaluation(succTxn: FullMVTurn) = new Reevaluation(succTxn, node)
}

class SourceReevaluation(override val turn: FullMVTurn, override val node: ReSource[FullMVStruct]) extends SourceReevaluationHandling {
  override def doCompute(): Unit = doReevaluation(retainBranch = true)
  override def toString = s"SourceReevaluation($turn, $node)"
}

trait SourceReevaluationHandling extends ReevaluationHandling[ReSource[FullMVStruct]] {
  def doReevaluation(retainBranch: Boolean): Unit = {
//    assert(Thread.currentThread() == turn.userlandThread, s"$this on different thread ${Thread.currentThread().getName}")
    assert(turn.phase == TurnPhase.Executing, s"$turn cannot source-reevaluate (requires executing phase")
    val ic = turn.asInstanceOf[FullMVTurnImpl].initialChanges(node)
    assert(ic.source eq node, s"$turn initial change map broken?")
    if(!ic.writeValue(ic.source.state.latestValue, x => {
      val res = processReevaluationResult(Some(x.asInstanceOf[node.Value]))
      processReevOutResult(retainBranch, res, changed = true)
    })) {
      val res = processReevaluationResult(None)
      processReevOutResult(retainBranch, res, changed = false)
    }
  }

  override def createReevaluation(succTxn: FullMVTurn): FullMVAction = new SourceReevaluation(succTxn, node)
}

trait ReevaluationHandling[N <: ReSource[FullMVStruct]] extends FullMVAction {
  def createReevaluation(succTxn: FullMVTurn): FullMVAction
  def doReevaluation(retainBranch: Boolean): Unit

  def processReevaluationResult(maybeChange: Option[node.Value]): ReevOutResult[FullMVTurn, Reactive[FullMVStruct]] = {
    val reevOutResult = node.state.reevOut(turn, maybeChange)
    if(FullMVEngine.DEBUG && maybeChange.isDefined && maybeChange.get.isInstanceOf[Pulse.Exceptional]){
      // could be a framework exception that is relevant to debugging, but was eaten by reactive's
      // exception propagation and thus wouldn't be reported otherwise..
      if(reevOutResult == Glitched) {
        println(s"[${Thread.currentThread().getName}] INFO: $this temporarily glitched result is exceptional:")
      } else {
        println(s"[${Thread.currentThread().getName}] WARNING: $this glitch-free result is exceptional:")
      }
      maybeChange.get.asInstanceOf[Pulse.Exceptional].throwable.printStackTrace()
    }
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] Reevaluation($turn,$node) => ${if(maybeChange.isDefined) "changed" else "unchanged"} $reevOutResult")
    reevOutResult
  }

  def processReevOutResult(retainBranch: Boolean, outAndSucc: ReevOutResult[FullMVTurn, Reactive[FullMVStruct]], changed: Boolean): Unit = {
    outAndSucc match {
      case Glitched =>
        // do nothing, reevaluation will be repeated at a later point
        if(!retainBranch) turn.activeBranchDifferential(TurnPhase.Executing, -1)
      case NoSuccessor(out) =>
        doBranchDiff(retainBranch, out)
        for(dep <- out) new Notification(turn, dep, changed).fork()
      case FollowFraming(out, succTxn) =>
        doBranchDiff(retainBranch, out)
        for(dep <- out) new NotificationWithFollowFrame(turn, dep, changed, succTxn).fork()
      case NextReevaluation(out, succTxn) =>
        doBranchDiff(retainBranch, out)
        for(dep <- out) new NotificationWithFollowFrame(turn, dep, changed, succTxn).fork()
        createReevaluation(succTxn).fork()
    }
  }

  private def doBranchDiff(retainBranch: Boolean, out: Set[Reactive[FullMVStruct]]) = {
    val branchDiff = out.size - (if(retainBranch) 1 else 2)
    if (branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Executing, branchDiff)
  }
}
