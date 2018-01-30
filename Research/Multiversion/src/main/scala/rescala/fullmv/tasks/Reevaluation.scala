package rescala.fullmv.tasks

import rescala.core.{ReSource, Interp, Reactive, ReevTicket, Result}
import rescala.fullmv.NotificationResultAction.NotificationOutAndSuccessorOperation.{FollowFraming, NextReevaluation, NoSuccessor}
import rescala.fullmv.NotificationResultAction.{Glitched, ReevOutResult}
import rescala.fullmv._

case class Reevaluation(override val turn: FullMVTurn, override val node: Reactive[FullMVStruct]) extends RegularReevaluationHandling {
  override def doCompute(): Unit = doReevaluation()
}

trait RegularReevaluationHandling extends ReevaluationHandling[Reactive[FullMVStruct]] {
  override val node: Reactive[FullMVStruct]
  def doReevaluation(): Unit = {
//    assert(Thread.currentThread() == turn.userlandThread, s"$this on different thread ${Thread.currentThread().getName}")
    assert(turn.phase == TurnPhase.Executing, s"$turn cannot reevaluate (requires executing phase")
    val ticket = new ReevTicket[node.Value, FullMVStruct](turn, node.state.reevIn(turn)) {
      override protected def staticAccess[A](reactive: Interp[A, FullMVStruct]): A = turn.staticAfter(reactive)
      override protected def dynamicAccess[A](reactive: Interp[A, FullMVStruct]): A = turn.dynamicAfter(reactive)
    }
    val res: Result[node.Value, FullMVStruct] = try {
      turn.host.withTurn(turn) {
        node.reevaluate(ticket)
      }
    } catch {
      case exception: Throwable =>
        System.err.println(s"[FullMV Error] Reevaluation of $node failed with ${exception.getClass.getName}: ${exception.getMessage}; Completing reevaluation as NoChange.")
        exception.printStackTrace()
        ticket.withPropagate(false)
    }
    res.getDependencies().foreach(commitDependencyDiff(node, node.state.incomings))
    var value = Option(node.state.reevIn(turn))
    res.forValue(v => value = Some(v))
    res.forEffect(_())
    processReevaluationResult(if(res.propagate) value else None)
  }

  final def commitDependencyDiff(node: Reactive[FullMVStruct], current: Set[ReSource[FullMVStruct]])(updated: Set[ReSource[FullMVStruct]]): Unit = {
    val indepsRemoved = current -- updated
    val indepsAdded = updated -- current
    indepsRemoved.foreach(turn.drop(_, node))
    indepsAdded.foreach(turn.discover(_, node))
    turn.writeIndeps(node, updated)
  }

  override def createReevaluation(succTxn: FullMVTurn) = Reevaluation(succTxn, node)
}

case class SourceReevaluation(override val turn: FullMVTurn, override val node: ReSource[FullMVStruct]) extends SourceReevaluationHandling {
  override def doCompute(): Unit = doReevaluation()
}

trait SourceReevaluationHandling extends ReevaluationHandling[ReSource[FullMVStruct]] {
  def doReevaluation(): Unit = {
//    assert(Thread.currentThread() == turn.userlandThread, s"$this on different thread ${Thread.currentThread().getName}")
    assert(turn.phase == TurnPhase.Executing, s"$turn cannot source-reevaluate (requires executing phase")
    val ic = turn.asInstanceOf[FullMVTurnImpl].initialChanges(node)
    assert(ic.source == node, s"$turn initial change map broken?")
    val res = ic.value.asInstanceOf[node.Value]
    processReevaluationResult(Some(res))
  }

  override def createReevaluation(succTxn: FullMVTurn): FullMVAction = SourceReevaluation(succTxn, node)
}

trait ReevaluationHandling[N <: ReSource[FullMVStruct]] extends FullMVAction {
  val node: N
  def createReevaluation(succTxn: FullMVTurn): FullMVAction
  def doReevaluation(): Unit

  def processReevaluationResult(maybeChange: Option[node.Value]): Unit = {
    val reevOutResult = node.state.reevOut(turn, maybeChange)
    processReevOutResult(reevOutResult, changed = maybeChange.isDefined)
  }

  def processReevOutResult(outAndSucc: ReevOutResult[FullMVTurn, Reactive[FullMVStruct]], changed: Boolean): Unit = {
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] Reevaluation($turn,$node) => ${if(changed) "changed" else "unchanged"} $outAndSucc")
    outAndSucc match {
      case Glitched =>
        // do nothing, reevaluation will be repeated at a later point
        turn.activeBranchDifferential(TurnPhase.Executing, -1)
      case NoSuccessor(out) =>
        if(out.size != 1) turn.activeBranchDifferential(TurnPhase.Executing, out.size - 1)
        for(dep <- out) Notification(turn, dep, changed).fork()
      case FollowFraming(out, succTxn) =>
        if(out.size != 1) turn.activeBranchDifferential(TurnPhase.Executing, out.size - 1)
        for(dep <- out) NotificationWithFollowFrame(turn, dep, changed, succTxn).fork()
      case NextReevaluation(out, succTxn) =>
        succTxn.activeBranchDifferential(TurnPhase.Executing, 1)
        if(out.size != 1) turn.activeBranchDifferential(TurnPhase.Executing, out.size - 1)
        for(dep <- out) NotificationWithFollowFrame(turn, dep, changed, succTxn).fork()
        createReevaluation(succTxn).fork()
    }
  }
}
