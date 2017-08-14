package rescala.fullmv.tasks

import rescala.core.{Node, Reactive}
import rescala.fullmv.FramingBranchResult.{FramingBranchEnd, FramingBranchOut, FramingBranchOutSuperseding}
import rescala.fullmv._

trait FramingTask extends FullMVAction {
  override def doCompute(): Unit = {
    val branchResult = doFraming()
    if(FullMVEngine.DEBUG) println(s"[${Thread.currentThread().getName}] $this => $branchResult")
    branchResult match {
      case FramingBranchEnd =>
        turn.activeBranchDifferential(TurnPhase.Framing, -1)
      case FramingBranchOut(out) =>
        val branchDiff = out.size - 1
        if(branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Framing, branchDiff)
        for(succ <- out) Framing(turn, succ).fork()
      case FramingBranchOutSuperseding(out, supersede) =>
        val branchDiff = out.size - 1
        if(branchDiff != 0) turn.activeBranchDifferential(TurnPhase.Framing, branchDiff)
        for(succ <- out) SupersedeFraming(turn, succ, supersede).fork()
    }

  }
  def doFraming(): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]]
}

case class Framing(turn: FullMVTurn, node: Node[FullMVStruct]) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
    node.state.incrementFrame(turn)
  }
}

case class SupersedeFraming(turn: FullMVTurn, node: Node[FullMVStruct], supersede: FullMVTurn) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
    assert(supersede.phase == TurnPhase.Framing, s"$supersede cannot have frame superseded (requires framing phase)")
    node.state.incrementSupersedeFrame(turn, supersede)
  }
}
