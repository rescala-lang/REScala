package rescala.fullmv.tasks

import rescala.core.ReSource
import rescala.fullmv.FramingBranchResult.{FramingBranchEnd, FramingBranchOut, FramingBranchOutSuperseding}
import rescala.fullmv._

trait FramingTask extends FullMVAction {
  override def doCompute(): Traversable[FullMVAction] = {
    val branchResult = doFraming()
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this => $branchResult")
    branchResult match {
      case FramingBranchEnd =>
        turn.activeBranchDifferential(TurnPhase.Framing, -1)
        Traversable.empty
      case FramingBranchOut(out) =>
        val branchDiff = out.size - 1
        if(branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Framing, branchDiff)
        out.map(Framing(turn, _))
      case FramingBranchOutSuperseding(out, supersede) =>
        val branchDiff = out.size - 1
        if(branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Framing, branchDiff)
        out.map(SupersedeFraming(turn, _, supersede))
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

case class SupersedeFraming(turn: FullMVTurn, node: ReSource[FullMVStruct], supersede: FullMVTurn) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, ReSource[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
    assert(supersede.phase == TurnPhase.Framing, s"$supersede cannot have frame superseded (requires framing phase)")
    node.state.incrementSupersedeFrame(turn, supersede)
  }
}
