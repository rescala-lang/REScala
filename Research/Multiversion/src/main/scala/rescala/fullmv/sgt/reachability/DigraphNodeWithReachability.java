package rescala.fullmv.sgt.reachability;

/*
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
// TODO must have remote reference counterpart
public class DigraphNodeWithReachability {
  // TODO must be serializable
  private static class SpanningTreeNode {
//    final SpanningTreeNode parent;
    // TODO must include remote refs
    final DigraphNodeWithReachability digraphNode;
    final Set<SpanningTreeNode> children = new java.util.HashSet<>();
//    final Set<SpanningTreeNode> children = java.util.concurrent.ConcurrentHashMap.newKeySet();
//    SpanningTreeNode(SpanningTreeNode parent, DigraphNodeWithReachability digraphNode) {
//      this.parent = parent;
    SpanningTreeNode(DigraphNodeWithReachability digraphNode) {
      this.digraphNode = digraphNode;
    }
  }

  // TODO must include remote refs
  private final Collection<DigraphNodeWithReachability> predecessorsIncludingSelf = new java.util.ArrayList<>();
  private final Map<DigraphNodeWithReachability, SpanningTreeNode> successorSpanningTreeNodes = new java.util.concurrent.ConcurrentHashMap<>();
  private final SpanningTreeNode selfNode = new SpanningTreeNode(this);
//  private final SpanningTreeNode selfNode = new SpanningTreeNode(null, this);

  public DigraphNodeWithReachability() {
    predecessorsIncludingSelf.add(this);
    successorSpanningTreeNodes.put(this, selfNode);
  }

  public boolean isReachable(DigraphNodeWithReachability node) {
    assert hasNotBeenDiscarded();
    return successorSpanningTreeNodes.containsKey(node);
  }

//  private void copySubTreeRootAndAssessChildren(SpanningTreeNode attachBelow, SpanningTreeNode spanningSubTreeRoot) {
//    assert attachBelow == successorSpanningTreeNodes.get(attachBelow.digraphNode);
  private void copySubTreeRootAndAssessChildren(DigraphNodeWithReachability attachBelow, SpanningTreeNode spanningSubTreeRoot) {
    DigraphNodeWithReachability newNode = spanningSubTreeRoot.digraphNode;
    newNode.predecessorsIncludingSelf.add(this);

//    SpanningTreeNode copiedSpanningTreeNode = new SpanningTreeNode(attachBelow, newNode);
    SpanningTreeNode copiedSpanningTreeNode = new SpanningTreeNode(newNode);
    successorSpanningTreeNodes.put(newNode, copiedSpanningTreeNode);
    successorSpanningTreeNodes.get(attachBelow).children.add(copiedSpanningTreeNode);
//    successorSpanningTreeNodes.get(attachBelow.digraphNode).children.add(copiedSpanningTreeNode);

    for(SpanningTreeNode child : spanningSubTreeRoot.children){
      maybeNewReachableSubtree(newNode, child);
//      maybeNewReachableSubtree(copiedSpanningTreeNode, child);
    }
  }

//  private void maybeNewReachableSubtree(SpanningTreeNode attachBelow, SpanningTreeNode spanningSubTreeRoot) {
  // TODO remote callable
  // TODO should collect newly reachable nodes, and top level call should broadcast them to enable local mirroring; requires pointer to FullMVTurn or class merge
  private void maybeNewReachableSubtree(DigraphNodeWithReachability attachBelow, SpanningTreeNode spanningSubTreeRoot) {
    if(!isReachable(spanningSubTreeRoot.digraphNode)) {
      copySubTreeRootAndAssessChildren(attachBelow, spanningSubTreeRoot);
    }
  }

  private boolean hasNotBeenDiscarded() {
    return successorSpanningTreeNodes.containsKey(this);
  }

  // TODO remote callable
  public boolean addSuccessor(DigraphNodeWithReachability to) {
    assert hasNotBeenDiscarded();
    boolean isNew = !isReachable(to);
    if (isNew) {
      for(DigraphNodeWithReachability predecessorOrSelf: predecessorsIncludingSelf) {
//        predecessor.maybeNewReachableSubtree(selfNode, to.selfNode);
        // TODO possible remote call
        predecessorOrSelf.maybeNewReachableSubtree(this, to.selfNode);
      }
    }
    return isNew;
  }

//  private void discardedSuccessor(DigraphNodeWithReachability node) {
//    SpanningTreeNode removed = successorSpanningTreeNodes.remove(node);
//    removed.parent.children.remove(removed);
//  }

  public void discard() {
    assert hasNotBeenDiscarded();
    // unlink all spanning tree nodes to allow other referenced nodes to be GC'd
    successorSpanningTreeNodes.clear();
    selfNode.children.clear();
//    for(DigraphNodeWithReachability predecessor: predecessorsIncludingSelf) {
//      discardedSuccessor(this);
//    }
//    predecessorsIncludingSelf.clear();
  }
}
