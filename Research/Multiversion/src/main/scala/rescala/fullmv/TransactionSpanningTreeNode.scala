package rescala.fullmv

class TransactionSpanningTreeNode[T] (val txn: T) {
  var children: Set[TransactionSpanningTreeNode[T]] = Set.empty
  def map[B](f: T => B): TransactionSpanningTreeNode[B] = {
    val res = new TransactionSpanningTreeNode(f(txn))
    res.children = children.map(_.map(f))
    res
  }
}
