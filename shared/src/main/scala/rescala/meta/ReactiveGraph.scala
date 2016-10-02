package rescala.meta

class ReactiveGraph {
  private val nodes : collection.mutable.Set[ReactiveNode[_]] = collection.mutable.Set()
  private val log : collection.mutable.MutableList[MetaLog[_]] = collection.mutable.MutableList()

  def numNodes = nodes.size

  protected[meta] def addLog(newLog : MetaLog[_]) = log += newLog
  protected[meta] def popLog() : List[MetaLog[_]] = {
    val l = log.toList
    log.clear()
    l
  }

  def createVar[A]() : VarSignalPointer[A] = {
    val node = new ReactiveNode(this, Set())
    registerReactiveNode(node)
    VarSignalPointer[A](node)
  }

  def createEvt[T]() : EvtEventPointer[T] = {
    val node = new ReactiveNode(this, Set())
    registerReactiveNode(node)
    EvtEventPointer[T](node)
  }

  protected[meta] def createReactiveNode[T](initDependencies : Set[ReactiveNode[_]] = Set()): ReactiveNode[T] = {
    val node = new ReactiveNode(this, initDependencies)
    registerReactiveNode(node)
    node
  }

  private def registerReactiveNode[T](reactive: ReactiveNode[T]) : Unit = {
    nodes += reactive
    for (incoming <- reactive.dependencies.diff(nodes)) {
      registerReactiveNode(incoming)
    }
  }

  protected[meta] def mergeNodes[T](mergeNodes : Set[ReactiveNode[T]]): ReactiveNode[T] = {
    val mergeSet = Set[ReactiveNode[_]]() ++ mergeNodes
    if (mergeSet.diff(nodes).nonEmpty) throw new IllegalArgumentException("Can only merge nodes within the same graph!")
    if (log.exists(n => mergeSet.contains(n.node))) throw new IllegalArgumentException("Can not merge nodes that have already a non-empty event log!")

    val mergedDependencies = mergeNodes.foldLeft(Set[ReactiveNode[_]]())((acc, next) => acc.union(next.dependencies)) -- mergeNodes
    val mergedNode = new ReactiveNode(this, mergedDependencies)

    for (node <- nodes -- mergeNodes if node.dependencies.intersect(mergeSet).nonEmpty) {
      node.setDependencies(node.dependencies -- mergeNodes)
      node.addDependency(mergedNode)
    }

    nodes --= mergeNodes
    nodes += mergedNode

    mergedNode
  }

  def incoming(node : ReactiveNode[_]) = node.dependencies
  def outgoing(node : ReactiveNode[_]) = nodes.filter(_.dependencies.contains(node))

  private def findDominators(node : ReactiveNode[_]) : Set[ReactiveNode[_]] = {
    incoming(node).foldLeft(nodes.toSet)((doms, pred) => doms.intersect(findDominators(pred))) + node
  }

  private def findPostdominators(node: ReactiveNode[_]) : Set[ReactiveNode[_]] = {
    outgoing(node).foldLeft(nodes.toSet)((doms, pred) => doms.intersect(findPostdominators(pred))) + node
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

class ReactiveNode[+T](protected[meta] val graph : ReactiveGraph, initDependencies : Set[ReactiveNode[_]] = Set()) {
  private val _dependencies : collection.mutable.Set[ReactiveNode[_]] = collection.mutable.Set() ++ initDependencies

  def dependencies = Set() ++ _dependencies

  protected[meta] def addDependency(reactiveNode: ReactiveNode[_]) : Unit = _dependencies += reactiveNode

  protected[meta] def dropDependency(reactiveNode: ReactiveNode[_]) : Unit = _dependencies -= reactiveNode

  protected[meta] def setDependencies(dependencies: Set[ReactiveNode[_]]) : Unit = {
    _dependencies.clear()
    _dependencies ++= dependencies
  }
}
