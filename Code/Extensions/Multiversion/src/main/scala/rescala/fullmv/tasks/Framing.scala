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
        for(dep <- out) new Framing(maybeOtherTurn, dep).fork
      case FrameSupersede(out, maybeOtherTurn, supersede) =>
        branchCountDiffOnBranchOut(out, maybeOtherTurn)
        for(dep <- out) new SupersedeFraming(maybeOtherTurn, dep, supersede).fork
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

class Framing(override val turn: FullMVTurn, override val node: ReSource[FullMVStruct]) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, ReSource[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
    node.state.incrementFrame(turn)
  }
  override def toString = s"Framing($turn, $node)"
}

class SupersedeFraming(override val turn: FullMVTurn, override val node: ReSource[FullMVStruct], supersede: FullMVTurn) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, ReSource[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
    assert(supersede.phase == TurnPhase.Framing, s"$supersede cannot have frame superseded (requires framing phase)")
    node.state.incrementSupersedeFrame(turn, supersede)
  }
  override def toString = s"Framing($turn, $node)"
}
