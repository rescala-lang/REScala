package rescala.meta

import scala.collection.mutable

class DataFlowGraph {
  private val _nodes : mutable.Set[DataFlowNode[_]] = mutable.Set()
  private val _log : mutable.Queue[MetaLog[_]] = mutable.Queue()

  protected[meta] def nodes = _nodes.toSet
  protected[meta] def log = _log.toList

  def numNodes = _nodes.size

  protected[meta] def addLog(newLog : MetaLog[_]) = _log += newLog
  protected[meta] def popLog() : List[MetaLog[_]] = {
    val l = _log.toList
    _log.clear()
    l
  }

  protected[meta] def moveNodes(moveNodes : Set[DataFlowNode[_]], newGraph : DataFlowGraph): Unit = {
    if (moveNodes.exists(!_nodes.contains(_)))
      throw new IllegalArgumentException("Cannot move nodes from other graphs!")
    if (moveNodes.exists(_.dependencies.exists(dep => !moveNodes.contains(dep))))
      throw new IllegalArgumentException("Cannot move a non-independent set of nodes to another reactive graph!")

    _nodes --= moveNodes
    val extractedLogs = _log.filter(l => moveNodes.contains(l.node))
    val remainingLogs = _log.filter(l => !extractedLogs.contains(l))
    _log.clear()
    _log ++= remainingLogs
    newGraph._nodes ++= moveNodes
    moveNodes.foreach(_.graph = newGraph)
    newGraph._log ++= extractedLogs
  }

  def createVar[A]() : VarPointer[A] = new VarPointer(VarSignalNode[A](this))

  def createEvt[T]() : EvtPointer[T] = new EvtPointer(EvtEventNode[T](this))

  protected[meta] def registerNode[T](reactive: DataFlowNode[T]) : Unit = {
    _nodes += reactive
    addLog(LoggedCreate(reactive))
    for (incoming <- reactive.dependencies.diff(_nodes)) {
      registerNode(incoming)
    }
  }

  protected[meta] def incomingDependencies(node : DataFlowNode[_]) = node.dependencies
  protected[meta] def outgoingDependencies(node : DataFlowNode[_]) = _nodes.filter(_.dependencies.contains(node))

  private def findDominators(node : DataFlowNode[_]) : Set[DataFlowNode[_]] = {
    incomingDependencies(node).foldLeft(_nodes.toSet)((doms, pred) => doms.intersect(findDominators(pred))) + node
  }

  private def findPostdominators(node: DataFlowNode[_]) : Set[DataFlowNode[_]] = {
    outgoingDependencies(node).foldLeft(_nodes.toSet)((doms, pred) => doms.intersect(findPostdominators(pred))) + node
  }

  private def findEnclosedNodes(current: DataFlowNode[_], exit: DataFlowNode[_]) : List[DataFlowNode[_]] = {
    if (current == exit) List(exit)
    else outgoingDependencies(current).foldLeft(List[DataFlowNode[_]]())(_ ++ findEnclosedNodes(_, exit)) :+ current
  }

  private def isSingleEntryExitArea(entry: DataFlowNode[_], exit: DataFlowNode[_]) : Boolean = {
    val dom = findDominators(exit)
    val postdom = findPostdominators(entry)
    dom.contains(entry) && postdom.contains(exit)
  }
}


