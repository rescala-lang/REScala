package rescala.fullmv

trait TransactionSpanningTreeNode[T]{
  def txn: T
  def children: Set[_ <: TransactionSpanningTreeNode[T]]
  def map[B](f: T => B): CaseClassTransactionSpanningTreeNode[B] = CaseClassTransactionSpanningTreeNode(f(txn), children.map(_.map(f)))
}

case class CaseClassTransactionSpanningTreeNode[T](txn: T, children: Set[CaseClassTransactionSpanningTreeNode[T]]) extends TransactionSpanningTreeNode[T]

class MutableTransactionSpanningTreeNode[T](override val txn: T) extends TransactionSpanningTreeNode[T] {
  var children: Set[TransactionSpanningTreeNode[T]] = Set.empty
}
