package rescala.meta

class ReactiveGraph {
  private val nodes : collection.mutable.Set[ReactiveNode] = collection.mutable.Set()

  def createVar[A]() : VarSignalPointer[A] = {
    val node = new ReactiveNode(this, Set())
    nodes += node
    VarSignalPointer[A](node)
  }

  def createEvt[T]() : EvtEventPointer[T] = {
    val node = new ReactiveNode(this, Set())
    nodes += node
    EvtEventPointer[T](node)
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
}

class ReactiveNode(protected[meta] val graph : ReactiveGraph, initDependencies : Set[ReactiveNode] = Set()) {
  private val _dependencies : collection.mutable.Set[ReactiveNode] = collection.mutable.Set() ++ initDependencies

  def dependencies = Set() ++ _dependencies

  def addDependency(reactiveNode: ReactiveNode) : Unit = _dependencies += reactiveNode

  def dropDependency(reactiveNode: ReactiveNode) : Unit = _dependencies -= reactiveNode

  def setDependencies(dependencies: Set[ReactiveNode]) : Unit = {
    _dependencies.clear()
    _dependencies ++= dependencies
  }
}

trait ManagedReactive[+T] {
  val node : ReactiveNode
}

