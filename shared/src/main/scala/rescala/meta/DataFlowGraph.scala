package rescala.meta

import scala.collection.mutable

class DataFlowGraph {
  private val _nodes : mutable.Set[DataFlowNode[_]] = mutable.Set()
  private val _log : mutable.Queue[MetaLog[_]] = mutable.Queue()
  private val _refs : mutable.Map[DataFlowRef[_], DataFlowNode[_]] = mutable.Map()

  protected[meta] def nodes = _nodes.toSet
  protected[meta] def log = _log.toList
  protected[meta] def refs = _refs.toMap

  def numNodes = _nodes.size
  def deleteNode(toDelete : DataFlowNode[_]): Unit = {
    if (pointersForNode(toDelete).nonEmpty) throw new IllegalArgumentException("Cannot delete a node that is still referenced!")
    _nodes -= toDelete
  }

  protected[meta] def addLog(newLog : MetaLog[_]) = _log += newLog
  protected[meta] def popLog() : List[MetaLog[_]] = {
    val l = _log.toList
    _log.clear()
    l
  }

  protected[meta] def resolvePointer[T](pointer: DataFlowRef[T]): Option[DataFlowNode[T]] = _refs.get(pointer) match {
    case Some(p) => Some(p.asInstanceOf[DataFlowNode[T]])
    case None => None
  }
  protected[meta] def pointersForNode[T](node: DataFlowNode[T]) = _refs.filter(_._2 == node).keySet
  protected[meta] def registerPointer[T](pointer: DataFlowRef[T], node: DataFlowNode[T]) = _refs += (pointer -> node)
  protected[meta] def deletePointer(pointer: DataFlowRef[_]) = _refs -= pointer

  protected[meta] def moveNodes(moveNodes : Set[DataFlowNode[_]], newGraph : DataFlowGraph): Unit = {
    if (moveNodes.exists(!_nodes.contains(_)))
      throw new IllegalArgumentException("Cannot move nodes from other graphs!")
    if (moveNodes.exists(_.dependencies.exists(dep => !moveNodes.contains(_refs.getOrElse(dep, null)))))
      throw new IllegalArgumentException("Cannot move a non-independent set of nodes to another reactive graph!")

    _nodes --= moveNodes
    val extractedLogs = _log.filter(l => moveNodes.contains(resolvePointer(l.node).orNull))
    val remainingLogs = _log.filter(l => !extractedLogs.contains(l))
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

  def createVar[A]() : VarRef[A] = new VarRef(VarSignalNode[A](this))

  def createEvt[T]() : EvtRef[T] = new EvtRef(EvtEventNode[T](this))

  protected[meta] def registerNode[T](reactive: DataFlowNode[T]) : Unit = {
    if (reactive.dependencies.exists(!_refs.contains(_))) throw new IllegalArgumentException("Cannot register node that has dependencies not found in the same data flow graph!")
      _nodes += reactive
    addLog(LoggedCreate(reactive.createRef()))
  }

  protected[meta] def incomingDependencies(node : DataFlowNode[_]) = node.dependencies.map(refs.get).collect{ case Some(n) => n }
  protected[meta] def outgoingDependencies(node : DataFlowNode[_]) = nodes.filter(_.dependencies.collect{ case DataFlowRef(n) => n }.contains(node))
}


