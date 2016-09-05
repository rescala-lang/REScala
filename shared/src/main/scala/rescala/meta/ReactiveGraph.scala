package rescala.meta

class ReactiveGraph {
  private val nodes : collection.mutable.Set[ReactiveNode] = collection.mutable.Set()
  private val log : collection.mutable.MutableList[MetaLog] = collection.mutable.MutableList()

  protected[meta] def addLog(newLog : MetaLog) = log += newLog

  def createVar[A]() : VarSignalPointer[A] = {
    val node = new ReactiveNode(this, Set())
    registerReactiveNode(node)
    VarSignalPointer[A](Some(node))
  }

  def createEvt[T]() : EvtEventPointer[T] = {
    val node = new ReactiveNode(this, Set())
    registerReactiveNode(node)
    EvtEventPointer[T](Some(node))
  }

  protected[meta] def createReactiveNode[T](initDependencies : Set[ReactiveNode] = Set()): ReactiveNode = {
    val node = new ReactiveNode(this, initDependencies)
    registerReactiveNode(node)
    node
  }

  private def registerReactiveNode[T](reactive: ReactiveNode) : Unit = {
    nodes += reactive
    for (incoming <- reactive.dependencies.diff(nodes)) {
      registerReactiveNode(incoming)
    }
  }

  protected[meta] def mergeNodes(mergeNodes : Set[ReactiveNode]): ReactiveNode = {
    if (mergeNodes.diff(nodes).nonEmpty) throw new IllegalArgumentException("Can only merge nodes within the same graph!")
    if (log.exists(n => mergeNodes.contains(n.node))) throw new IllegalArgumentException("Can not merge nodes that have already a non-empty event log!")

    val mergedDependencies = mergeNodes.foldLeft(Set[ReactiveNode]())((acc, next) => acc.union(next.dependencies)) -- mergeNodes
    val mergedNode = new ReactiveNode(this, mergedDependencies)

    for (node <- nodes -- mergeNodes if node.dependencies.intersect(mergeNodes).nonEmpty) {
      node.setDependencies(node.dependencies -- mergeNodes)
      node.addDependency(mergedNode)
    }

    nodes --= mergeNodes
    nodes += mergedNode

    mergedNode
  }

  def incoming(node : ReactiveNode) = node.dependencies
  def outgoing(node : ReactiveNode) = nodes.filter(_.dependencies.contains(node))

  private def findDominators(node : ReactiveNode) : Set[ReactiveNode] = {
    incoming(node).foldLeft(nodes.toSet)((doms, pred) => doms.intersect(findDominators(pred))) + node
  }

  private def findPostdominators(node: ReactiveNode) : Set[ReactiveNode] = {
    outgoing(node).foldLeft(nodes.toSet)((doms, pred) => doms.intersect(findPostdominators(pred))) + node
  }

  private def findEnclosedNodes(current: ReactiveNode, exit: ReactiveNode) : List[ReactiveNode] = {
    if (current == exit) List(exit)
    else outgoing(current).foldLeft(List[ReactiveNode]())(_ ++ findEnclosedNodes(_, exit)) :+ current
  }

  private def isSingleEntryExitArea(entry: ReactiveNode, exit: ReactiveNode) : Boolean = {
    val dom = findDominators(exit)
    val postdom = findPostdominators(entry)
    dom.contains(entry) && postdom.contains(exit)
  }
}

class ReactiveNode(protected[meta] val graph : ReactiveGraph, initDependencies : Set[ReactiveNode] = Set()) {
  private val _dependencies : collection.mutable.Set[ReactiveNode] = collection.mutable.Set() ++ initDependencies

  def dependencies = Set() ++ _dependencies

  protected[meta] def addDependency(reactiveNode: ReactiveNode) : Unit = _dependencies += reactiveNode

  protected[meta] def dropDependency(reactiveNode: ReactiveNode) : Unit = _dependencies -= reactiveNode

  protected[meta] def setDependencies(dependencies: Set[ReactiveNode]) : Unit = {
    _dependencies.clear()
    _dependencies ++= dependencies
  }
}

trait ManagedReactive[+T] {
  val node : ReactiveNode
}

