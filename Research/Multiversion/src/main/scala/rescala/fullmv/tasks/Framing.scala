package rescala.fullmv.tasks

import rescala.core.ReSource
import rescala.fullmv.FramingBranchResult._
import rescala.fullmv._

trait FramingTask extends FullMVAction {
  override def doCompute(): Unit = {
    val branchResult = doFraming()
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this => $branchResult")
    branchResult match {
      case FramingBranchEnd =>
        turn.activeBranchDifferential(TurnPhase.Framing, -1)
      case Frame(out, maybeOtherTurn) =>
        branchCountDiffOnBranchOut(out, maybeOtherTurn)
        for(dep <- out) Framing(maybeOtherTurn, dep).fork
      case Deframe(out, maybeOtherTurn) =>
        branchCountDiffOnBranchOut(out, maybeOtherTurn)
        for(dep <- out) Deframing(maybeOtherTurn, dep).fork
      case FrameSupersede(out, maybeOtherTurn, supersede) =>
        branchCountDiffOnBranchOut(out, maybeOtherTurn)
        for(dep <- out) SupersedeFraming(maybeOtherTurn, dep, supersede).fork
      case DeframeReframe(out, maybeOtherTurn, reframe) =>
        branchCountDiffOnBranchOut(out, maybeOtherTurn)
        for(dep <- out) DeframeReframing(maybeOtherTurn, dep, reframe).fork
    }
  }

  private def branchCountDiffOnBranchOut(out: Set[ReSource[FullMVStruct]], maybeOtherTurn: FullMVTurn): Unit = {
    if (turn == maybeOtherTurn) {
      val branchDiff = out.size - 1
      if (branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Framing, branchDiff)
    } else {
      if (out.nonEmpty) maybeOtherTurn.activeBranchDifferential(TurnPhase.Framing, out.size)
      turn.activeBranchDifferential(TurnPhase.Framing, -1)
    }
  }

  def doFraming(): FramingBranchResult[FullMVTurn, ReSource[FullMVStruct]]
}

case class Framing(turn: FullMVTurn, node: ReSource[FullMVStruct]) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, ReSource[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
    node.state.incrementFrame(turn)
  }
}

case class Deframing(turn: FullMVTurn, node: ReSource[FullMVStruct]) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, ReSource[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot decrement frame (requires framing phase)")
    node.state.decrementFrame(turn)
  }
}

case class SupersedeFraming(turn: FullMVTurn, node: ReSource[FullMVStruct], supersede: FullMVTurn) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, ReSource[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
    assert(supersede.phase == TurnPhase.Framing, s"$supersede cannot have frame superseded (requires framing phase)")
    node.state.incrementSupersedeFrame(turn, supersede)
  }
}

case class DeframeReframing(turn: FullMVTurn, node: ReSource[FullMVStruct], reframe: FullMVTurn) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, ReSource[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot decrement frame (requires framing phase)")
    assert(reframe.phase == TurnPhase.Framing, s"$reframe cannot have frame reframed (requires framing phase)")
    node.state.decrementReframe(turn, reframe)
  }
}
