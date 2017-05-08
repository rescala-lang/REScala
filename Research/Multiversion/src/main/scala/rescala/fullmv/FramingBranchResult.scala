package rescala.fullmv

import rescala.graph.Reactive

sealed trait FramingBranchResult[+R] {
  val branchDelta: Int
  def submitToPool(txn: FullMVTurn, taskPool: TaskPool): Unit
}

object FramingBranchResult {
  case object FramingBranchEnd extends FramingBranchResult[Nothing] {
    override val branchDelta = -1
    override def submitToPool(txn: FullMVTurn, taskPool: TaskPool): Unit = {}
  }

  case class FramingBranchOut[R](out: Set[Reactive[FullMVStruct]]) extends FramingBranchResult[R] {
    override val branchDelta: Int = out.size - 1
    override def submitToPool(txn: FullMVTurn, taskPool: TaskPool): Unit = {
      out.foreach { node => taskPool.addFraming(node, txn) }
    }
  }

  case class FramingBranchOutSuperseding[R](out: Set[Reactive[FullMVStruct]], supersede: FullMVTurn) extends FramingBranchResult[R] {
    override val branchDelta: Int = out.size - 1
    override def submitToPool(txn: FullMVTurn, taskPool: TaskPool): Unit = {
      out.foreach { node => taskPool.addSupersedingFraming(node, txn, supersede) }
    }
  }
}
