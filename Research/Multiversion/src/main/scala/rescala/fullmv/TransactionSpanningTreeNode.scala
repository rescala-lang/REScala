package rescala.fullmv

class TransactionSpanningTreeNode[T] (val txn: T) {
  var children: Set[TransactionSpanningTreeNode[T]] = Set.empty
}
