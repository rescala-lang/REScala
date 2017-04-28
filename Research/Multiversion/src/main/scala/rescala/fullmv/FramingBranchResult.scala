package rescala.fullmv

import rescala.graph.Reactive

sealed trait FramingBranchResult[+R] {
  def processBranching(txn: FullMVTurn, taskPool: TaskPool): Unit
}

object FramingBranchResult {
  case object FramingBranchEnd extends FramingBranchResult[Nothing] {
    override def processBranching(txn: FullMVTurn, taskPool: TaskPool): Unit = {
//      txn.branches(-1)
    }
  }

  case class FramingBranchOut[R](out: Set[Reactive[FullMVStruct]]) extends FramingBranchResult[R] {
    override def processBranching(txn: FullMVTurn, taskPool: TaskPool): Unit = {
//      if (out.size != 1) txn.branches(out.size - 1)
      out.foreach { node => taskPool.addFraming(node, txn) }
    }
  }

  case class FramingBranchOutSuperseding[R](out: Set[Reactive[FullMVStruct]], supersede: FullMVTurn) extends FramingBranchResult[R] {
    override def processBranching(txn: FullMVTurn, taskPool: TaskPool): Unit = {
//      if (out.size != 1) txn.branches(out.size - 1)
      out.foreach { node => taskPool.addSupersedingFraming(node, txn, supersede) }
    }
  }
}
