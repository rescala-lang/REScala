package rescala.meta

import rescala.meta.reactives._

/**
  * Created by nico on 07/08/2016.
  */
class ReactiveGraph {
  private val nodes : collection.mutable.Set[ReactiveNode] = collection.mutable.Set()

  def createVar[A]() : ManagedVar[A, DummyStruct] = {
    val node = new ReactiveNode(this, Set())
    nodes += node
    new ManagedVarImpl[A](node)
  }
  def createEvt[T]() : ManagedEvt[T, DummyStruct] = {
    val node = new ReactiveNode(this, Set())
    nodes += node
    new ManagedEvtImpl[T](node)
  }

}

class ReactiveNode(private val graph : ReactiveGraph, dependencies : Set[ReactiveNode]) {
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

