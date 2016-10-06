package rescala.meta

import scala.collection.mutable

class ReactiveGraph {
  private val _nodes : mutable.Set[ReactiveNode[_]] = mutable.Set()
  private val _log : mutable.Queue[MetaLog[_]] = mutable.Queue()
  private val _pointers : mutable.Map[ReactiveNode[_], mutable.Set[MetaPointer[_]]] = mutable.Map()

  protected[meta] def nodes = _nodes.toSet
  protected[meta] def log = _log.toList

  def numNodes = _nodes.size

  protected[meta] def addLog(newLog : MetaLog[_]) = _log += newLog
  protected[meta] def popLog() : List[MetaLog[_]] = {
    val l = _log.toList
    _log.clear()
    l
  }

  protected[meta] def moveNodes(moveNodes : Set[ReactiveNode[_]], newGraph : ReactiveGraph): Unit = {
    if (moveNodes.exists(!_nodes.contains(_)))
      throw new IllegalArgumentException("Cannot move nodes from other graphs!")
    if (moveNodes.exists(_.dependencies.exists(dep => !moveNodes.contains(dep))))
      throw new IllegalArgumentException("Cannot move a non-independent set of nodes to another reactive graph!")

    _nodes --= moveNodes
    val extractedLogs = _log.filter(l => moveNodes.contains(l.node))
    val remainingLogs = _log.filter(l => !extractedLogs.contains(l))
    _log.clear()
    _log ++= remainingLogs
    val extractedPointers = _pointers.filter(p => moveNodes.contains(p._1))
    _pointers --= extractedPointers.keys
    newGraph._nodes ++= moveNodes
    moveNodes.foreach(_.graph = newGraph)
    newGraph._log ++= extractedLogs
    newGraph._pointers ++= extractedPointers
  }

  def createVar[A]() : VarSignalPointer[A] = {
    val node = new ReactiveNode[A](this, Set())
    registerReactiveNode(node)
    VarSignalPointer[A](node)
  }

  def createEvt[T]() : EvtEventPointer[T] = {
    val node = new ReactiveNode[T](this, Set())
    registerReactiveNode(node)
    EvtEventPointer[T](node)
  }

  protected[meta] def createReactiveNode[T](initDependencies : Set[ReactiveNode[_]] = Set()): ReactiveNode[T] = {
    val node = new ReactiveNode(this, initDependencies)
    registerReactiveNode(node)
    node
  }

  protected[meta] def addPointer[T](node: ReactiveNode[T], pointer: MetaPointer[T]): Unit = {
    _pointers.get(node) match {
      case Some(set) => set += pointer
      case None => _pointers += node -> mutable.Set(pointer)
    }
  }

  protected[meta] def registerReactiveNode[T](reactive: ReactiveNode[T]) : Unit = {
    _nodes += reactive
    addLog(LoggedCreate(reactive))
    for (incoming <- reactive.dependencies.diff(_nodes)) {
      registerReactiveNode(incoming)
    }
  }

  protected[meta] def mergeNodes[T](mergeNodes : Set[ReactiveNode[T]]): ReactiveNode[T] = {
    val mergeSet = Set[ReactiveNode[_]]() ++ mergeNodes
    if (mergeSet.diff(_nodes).nonEmpty) throw new IllegalArgumentException("Can only merge nodes within the same graph!")
    if (_log.exists(n => mergeSet.contains(n.node))) throw new IllegalArgumentException("Can not merge nodes that have already a non-empty event log!")

    val mergedDependencies = mergeNodes.foldLeft(Set[ReactiveNode[_]]())((acc, next) => acc.union(next.dependencies)) -- mergeNodes
    val mergedNode = new ReactiveNode(this, mergedDependencies)

    for (node <- _nodes -- mergeNodes if node.dependencies.intersect(mergeSet).nonEmpty) {
      node.setDependencies(node.dependencies -- mergeNodes)
      node.addDependency(mergedNode)
    }

    _nodes --= mergeNodes
    _nodes += mergedNode

    mergedNode
  }

  protected[meta] def pointers(node : ReactiveNode[_]) : Set[MetaPointer[_]] = {
    Set[MetaPointer[_]]() ++ _pointers.getOrElse(node, Set())
  }

  protected[meta] def incoming(node : ReactiveNode[_]) = node.dependencies
  protected[meta] def outgoing(node : ReactiveNode[_]) = _nodes.filter(_.dependencies.contains(node))

  private def findDominators(node : ReactiveNode[_]) : Set[ReactiveNode[_]] = {
    incoming(node).foldLeft(_nodes.toSet)((doms, pred) => doms.intersect(findDominators(pred))) + node
  }

  private def findPostdominators(node: ReactiveNode[_]) : Set[ReactiveNode[_]] = {
    outgoing(node).foldLeft(_nodes.toSet)((doms, pred) => doms.intersect(findPostdominators(pred))) + node
  }

  private def findEnclosedNodes(current: ReactiveNode[_], exit: ReactiveNode[_]) : List[ReactiveNode[_]] = {
    if (current == exit) List(exit)
    else outgoing(current).foldLeft(List[ReactiveNode[_]]())(_ ++ findEnclosedNodes(_, exit)) :+ current
  }

  private def isSingleEntryExitArea(entry: ReactiveNode[_], exit: ReactiveNode[_]) : Boolean = {
    val dom = findDominators(exit)
    val postdom = findPostdominators(entry)
    dom.contains(entry) && postdom.contains(exit)
  }
}

class ReactiveNode[+T](protected[meta] var graph : ReactiveGraph, initDependencies : Set[ReactiveNode[_]] = Set()) {
  private val _dependencies : collection.mutable.Set[ReactiveNode[_]] = collection.mutable.Set() ++ initDependencies

  def dependencies = Set() ++ _dependencies

  protected[meta] def addDependency(reactiveNode: ReactiveNode[_]) : Unit = _dependencies += reactiveNode

  protected[meta] def dropDependency(reactiveNode: ReactiveNode[_]) : Unit = _dependencies -= reactiveNode

  protected[meta] def setDependencies(dependencies: Set[ReactiveNode[_]]) : Unit = {
    _dependencies.clear()
    _dependencies ++= dependencies
  }
}
