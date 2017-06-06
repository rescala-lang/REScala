package rescala.fullmv.sgt.reachability;

/**
 * using scala, we could define SpanningTreeNode to just be a recursively typed Map.Entry from java's maps.
 * This way we technically don't need the SpanningTreeNode class
 */
//import scala.collection.JavaConverters
//
//object DigraphNodeWithReachability {
//  type Set[T] = java.util.Set[T]
//  type SpanningTreeNode = java.util.Map.Entry[DigraphNodeWithReachability, Set[SpanningTreeNode]]
//}
//class DigraphNodeWithReachability {
//  import DigraphNodeWithReachability._
//
//  var reachableFrom: Set[DigraphNodeWithReachability] = new java.util.HashSet()
//  reachableFrom.add(this)
//
//  private[this] val treeNodeIndex = new java.util.HashMap[DigraphNodeWithReachability, Set[SpanningTreeNode]]()
//  treeNodeIndex.put(this, new java.util.HashSet())
//
//  val self: SpanningTreeNode = treeNodeIndex.getNode(this) // if only this was visible
//
//  def isReachable(node: DigraphNodeWithReachability): Boolean = treeNodeIndex.containsKey(node)
//  def copySubTreeRootAndAssessChildren(spanningTreeParent: DigraphNodeWithReachability, subTreeRoot: SpanningTreeNode): Unit = {
//    val digraphNode = subTreeRoot.getKey
//    digraphNode.reachableFrom.add(this)
//    treeNodeIndex.put(digraphNode, new java.util.HashSet())
//    val copiedNode: SpanningTreeNode = treeNodeIndex.getNode(digraphNode) // if only this was visible
//    treeNodeIndex.get(spanningTreeParent).add(copiedNode)
//    for(child <- subTreeRoot.getValue) maybeNewReachableSubtree(digraphNode, child)
//  }
//  def maybeNewReachableSubtree(spanningTreeParent: DigraphNodeWithReachability, subTreeRoot: SpanningTreeNode): Unit = {
//    if(!isReachable(subTreeRoot.getKey)) {
//      copySubTreeRootAndAssessChildren(spanningTreeParent, subTreeRoot)
//    }
//  }
//  def addSuccessor(to: DigraphNodeWithReachability): Unit = {
//    if (!isReachable(to)) {
//      for(predecessor <- JavaConverters.collectionAsScalaIterable(reachableFrom)) {
//        predecessor.maybeNewReachableSubtree(this, to.self)
//      }
//    }
//  }
//}

import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 *  Incremental Digraph Reachability Datastructure based on:
 *  G.F. Italiano 1986. "Amortized Efficiency Of A Path Retrieval Data Structure"
 *  Modifications:
 *  - Converted to Object-Oriented nature
 *  - Added dynamically grown successors set to not require a global list of nodes.
 *  - added ability to mark nodes as discarded: Discarded nodes will not be copied over as transitive dependencies.
 *    Switching the commented-out lines will additionally eagerly un-link discarded nodes from all trees; this is
 *    currently disabled because it's probably not worth the effort, in particular because it can happen concurrently
 *    to order establishment, requiring either thread safe child collections for spanning tree nodes or synchronization
 *    of turn completion through their Subsumable .lock
 *  TODO get this to cooperate with distribution
 */
public class DigraphNodeWithReachability {
  private static class SpanningTreeNode {
//    final SpanningTreeNode parent;
    final DigraphNodeWithReachability digraphNode;
    final Set<SpanningTreeNode> spanningTreeSuccessors = new java.util.HashSet<>();
//    final Set<SpanningTreeNode> spanningTreeSuccessors = java.util.concurrent.ConcurrentHashMap.newKeySet();
//    SpanningTreeNode(SpanningTreeNode parent, DigraphNodeWithReachability digraphNode) {
//      this.parent = parent;
    SpanningTreeNode(DigraphNodeWithReachability digraphNode) {
      this.digraphNode = digraphNode;
    }
  }

  private final Collection<DigraphNodeWithReachability> predecessors = new java.util.ArrayList<>();
  private final Map<DigraphNodeWithReachability, SpanningTreeNode> successorSpanningTreeNodes = new java.util.concurrent.ConcurrentHashMap<>();
  private final SpanningTreeNode selfNode = new SpanningTreeNode(this);
//  private final SpanningTreeNode selfNode = new SpanningTreeNode(null, this);

  private boolean discarded = false;

  public DigraphNodeWithReachability() {
    predecessors.add(this);
    successorSpanningTreeNodes.put(this, selfNode);
  }

  public boolean isReachable(DigraphNodeWithReachability node) {
    return successorSpanningTreeNodes.containsKey(node);
  }

//  private void copySubTreeRootAndAssessChildren(SpanningTreeNode spanningTreeParent, SpanningTreeNode spanningSubTreeRoot) {
//    assert spanningTreeParent == successorSpanningTreeNodes.get(spanningTreeParent.digraphNode);
  private void copySubTreeRootAndAssessChildren(DigraphNodeWithReachability spanningTreeParent, SpanningTreeNode spanningSubTreeRoot) {
    DigraphNodeWithReachability newNode = spanningSubTreeRoot.digraphNode;
    newNode.predecessors.add(this);

//    SpanningTreeNode copiedSpanningTreeNode = new SpanningTreeNode(spanningTreeParent, newNode);
    SpanningTreeNode copiedSpanningTreeNode = new SpanningTreeNode(newNode);
    successorSpanningTreeNodes.put(newNode, copiedSpanningTreeNode);
    successorSpanningTreeNodes.get(spanningTreeParent).spanningTreeSuccessors.add(copiedSpanningTreeNode);
//    successorSpanningTreeNodes.get(spanningTreeParent.digraphNode).spanningTreeSuccessors.add(copiedSpanningTreeNode);

    for(SpanningTreeNode child : spanningSubTreeRoot.spanningTreeSuccessors){
      maybeNewReachableSubtree(newNode, child);
//      maybeNewReachableSubtree(copiedSpanningTreeNode, child);
    }
  }

//  private void maybeNewReachableSubtree(SpanningTreeNode spanningTreeParent, SpanningTreeNode spanningSubTreeRoot) {
  private void maybeNewReachableSubtree(DigraphNodeWithReachability spanningTreeParent, SpanningTreeNode spanningSubTreeRoot) {
    if(!spanningSubTreeRoot.digraphNode.discarded && !isReachable(spanningSubTreeRoot.digraphNode)) {
      copySubTreeRootAndAssessChildren(spanningTreeParent, spanningSubTreeRoot);
    }
  }

  public void addSuccessor(DigraphNodeWithReachability to) {
    if (!isReachable(to)) {
      for(DigraphNodeWithReachability predecessor: predecessors) {
//        predecessor.maybeNewReachableSubtree(selfNode, to.selfNode);
        predecessor.maybeNewReachableSubtree(this, to.selfNode);
      }
    }
  }

//  private void discardedSuccessor(DigraphNodeWithReachability node) {
//    SpanningTreeNode removed = successorSpanningTreeNodes.remove(node);
//    removed.parent.spanningTreeSuccessors.remove(removed);
//  }

  public void discard() {
    discarded = true;
//    for(DigraphNodeWithReachability predecessor: predecessors) {
//      discardedSuccessor(this);
//    }
  }
}
