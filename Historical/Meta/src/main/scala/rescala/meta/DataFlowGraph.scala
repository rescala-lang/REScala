package reactives.meta

import reactives.core.Struct

import scala.collection.mutable

class DataFlowGraph {
  private val _nodes: mutable.Set[DataFlowNode[_]]                = mutable.Set()
  private val _log: mutable.Queue[MetaLog[_]]                     = mutable.Queue()
  private val _refs: mutable.Map[DataFlowRef[_], DataFlowNode[_]] = mutable.Map()

  // Components of the graph
  protected[meta] def nodes: Set[DataFlowNode[_]]                = _nodes.toSet
  protected[meta] def log: List[MetaLog[_]]                      = _log.toList
  protected[meta] def refs: Map[DataFlowRef[_], DataFlowNode[_]] = _refs.toMap

  // Non-destructive graph analysis methods
  def numNodes: Int = _nodes.size
  protected[meta] def deref[T](pointer: DataFlowRef[T]): Option[DataFlowNode[T]] =
    _refs.get(pointer) match {
      case Some(p) => Some(p.asInstanceOf[DataFlowNode[T]])
      case None    => None
    }
  protected[meta] def incomingDataFlow(node: DataFlowNode[_]): Set[DataFlowNode[_]] =
    node.dependencies.map(_refs.get).collect { case Some(n) => n }
  protected[meta] def outgoingDataFlow(node: DataFlowNode[_]): Set[DataFlowNode[_]] =
    nodes.filter(_.dependencies.collect { case DataFlowRef(n) => n }.contains(node))
  protected[meta] def isDataFlowPossible(from: DataFlowNode[_], to: DataFlowNode[_]): Boolean = {
    from == to || incomingDataFlow(to).exists(n => isDataFlowPossible(from, n))
  }

  // Graph modification (should be as access-restricted as possible)
  protected[meta] def registerNode[T](reactive: DataFlowNode[T]): Unit = {
    if (reactive.dependencies.exists(!_refs.contains(_)))
      throw new IllegalArgumentException(
        "Cannot register node that has dependencies not found in the same data flow graph!"
      )
    _nodes += reactive
    addLog(LoggedCreate(reactive.newRef()))
  }
  protected[meta] def deleteNode(toDelete: DataFlowNode[_]): Unit = {
    if (nodeRefs(toDelete).nonEmpty)
      throw new IllegalArgumentException("Cannot delete a node that is still referenced!")
    _nodes -= toDelete
  }

  protected[meta] def addLog(newLog: MetaLog[_]): Unit = _log += newLog
  protected[meta] def removeLog(metaLog: MetaLog[_]): Unit =
    metaLog match {
      case LoggedFire(_, _) | LoggedSet(_, _) => _log.dequeueAll(_ == metaLog)
      case _ =>
        throw new IllegalArgumentException("Cannot remove structural logs without causing graph inconsistencies!")

    }
  protected[meta] def nodeRefs[T](node: DataFlowNode[T]): Set[DataFlowRef[_]] = _refs.filter(_._2 == node).keySet.toSet
  protected[meta] def registerRef[T](
      pointer: DataFlowRef[T],
      node: DataFlowNode[T]
  ): mutable.Map[DataFlowRef[_], DataFlowNode[_]] = _refs += (pointer -> node)
  protected[meta] def deleteRef(pointer: DataFlowRef[_]): mutable.Map[DataFlowRef[_], DataFlowNode[_]] =
    _refs -= pointer

  protected[meta] def moveNodes(moveNodes: Set[DataFlowNode[_]], newGraph: DataFlowGraph): Unit = {
    if (moveNodes.exists(!_nodes.contains(_)))
      throw new IllegalArgumentException("Cannot move nodes from other graphs!")
    if (moveNodes.exists(_.dependencies.exists(dep => !moveNodes.contains(_refs.getOrElse(dep, null)))))
      throw new IllegalArgumentException("Cannot move a non-independent set of nodes to another reactive graph!")

    _nodes --= moveNodes
    val extractedLogs     = _log.filter(l => moveNodes.contains(deref(l.node).orNull))
    val remainingLogs     = _log.filter(l => !extractedLogs.contains(l))
    val extractedPointers = _refs.filter(n => moveNodes.contains(n._2))
    _refs --= extractedPointers.keys
    _log.clear()
    _log ++= remainingLogs
    newGraph._nodes ++= moveNodes
    moveNodes.foreach(_.graph = newGraph)
    newGraph._log ++= extractedLogs
    _refs.keys.foreach(_.graph = newGraph)
    newGraph._refs ++= extractedPointers
  }

  // Methods to create new root nodes in the graph. Only simplify access to public constructors, so access is non-critical
  def createVar[A](): VarRef[A] = new VarRef(VarSignalNode[A](this))
  def createVar[A, S <: Struct](v: A)(implicit reifier: Reifier[S]): VarRef[A] = {
    val newVar = new VarRef(VarSignalNode[A](this))
    newVar.set(v)
    newVar
  }
  def createEvt[T](): EvtRef[T] = new EvtRef(EvtEventNode[T](this))
}
