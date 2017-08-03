package rescala.fullmv;

import java.util.Set;

public class TransactionSpanningTreeNode<T> {
    // TODO must include remote refs
    final T txn;
    final Set<TransactionSpanningTreeNode<T>> children = new java.util.HashSet<>();
//    final Set<TransactionSpanningTreeNode> children = java.util.concurrent.ConcurrentHashMap.newKeySet();
//    TransactionSpanningTreeNode(TransactionSpanningTreeNode parent, DigraphNodeWithReachability digraphNode) {
//      this.parent = parent;
    TransactionSpanningTreeNode(T txn) {
      this.txn = txn;
    }
}
