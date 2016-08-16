package rescala.meta

class ReactiveGraph {
  private val nodes : collection.mutable.Set[ReactiveNode] = collection.mutable.Set()

  def createVar[A]() : VarSignalPointer[A] = {
    val node = new ReactiveNode(this, Set())
    nodes += node
    VarSignalPointer[A](Some(node))
  }

  def createEvt[T]() : EvtEventPointer[T] = {
    val node = new ReactiveNode(this, Set())
    nodes += node
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

