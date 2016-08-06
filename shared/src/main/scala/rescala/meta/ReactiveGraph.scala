package rescala.meta

/**
  * Created by nico on 07/08/2016.
  */
class ReactiveGraph {
  private val nodes : Set[ReactiveNode] = Set()
}

class ReactiveNode(dependencies : Set[ReactiveNode]) {
  private val incomingDependencies : collection.mutable.Set[ReactiveNode] = collection.mutable.Set() ++ dependencies

  def addDependency(reactiveNode: ReactiveNode) : Unit = incomingDependencies += reactiveNode

  def dropDependency(reactiveNode: ReactiveNode) : Unit = incomingDependencies -= reactiveNode

  def setDependencies(dependencies: Set[ReactiveNode]) : Unit = {
    incomingDependencies.clear()
    incomingDependencies ++= dependencies
  }
}

trait ManagedReactive {
  val node : ReactiveNode
}

