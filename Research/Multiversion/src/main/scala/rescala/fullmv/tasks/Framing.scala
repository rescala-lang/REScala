package rescala.fullmv.tasks

import rescala.core.Reactive
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
        turn.activeBranchDifferential(TurnPhase.Framing, out.size - 1)
        for(succ <- out) Framing(turn, succ).fork()
      case FramingBranchOutSuperseding(out, supersede) =>
        turn.activeBranchDifferential(TurnPhase.Framing, out.size - 1)
        for(succ <- out) SupersedeFraming(turn, succ, supersede).fork()
    }

  }
  def doFraming(): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]]
}

case class Framing(turn: FullMVTurn, node: Reactive[FullMVStruct]) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
    node.state.incrementFrame(turn)
  }
}

case class SupersedeFraming(turn: FullMVTurn, node: Reactive[FullMVStruct], supersede: FullMVTurn) extends FramingTask {
  override def doFraming(): FramingBranchResult[FullMVTurn, Reactive[FullMVStruct]] = {
    assert(turn.phase == TurnPhase.Framing, s"$this cannot increment frame (requires framing phase)")
    assert(supersede.phase == TurnPhase.Framing, s"$supersede cannot have frame superseded (requires framing phase)")
    node.state.incrementSupersedeFrame(turn, supersede)
  }
}
