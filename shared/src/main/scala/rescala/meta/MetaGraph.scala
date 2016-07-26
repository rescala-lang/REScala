package rescala.meta

import rescala.engines.Ticket
import rescala.graph.{Pulse, Reactive, Struct}

/*trait ReactiveGraph[S <: Struct] {
  def newRootElement() : ReactiveElement[S]
  protected[meta] def registerElement(reactiveElement: ReactiveElement[S]) : Unit
  protected[meta] def createBudForElement(reactiveElement: ReactiveElement[S]) : S#Spore[Reactive[S]]
  def ticket : Ticket[S]
}

trait ReactiveElement[S <: Struct] {
  def newDependentElement() : ReactiveElement[S]
  def dependencies : Set[ReactiveElement[S]]
  def instance : Reactive[S]
  protected[meta] def bud : S#Spore[Reactive[S]]
  def hasInstances : Boolean
}

class ReactiveGraphImpl[S <: Struct](implicit val ticket: Ticket[S]) extends ReactiveGraph[S] {
  private val nodes = scala.collection.mutable.Set[ReactiveElement[S]]()

  override def newRootElement() : ReactiveElement[S] = {
    val newRoot = new ReactiveElementImpl[S](this)
    newRoot
  }

  override protected[meta] def registerElement(reactiveElement: ReactiveElement[S]): Unit = {
    nodes += reactiveElement
  }

  override protected[meta] def createBudForElement(reactiveElement: ReactiveElement[S]): S#Spore[Reactive[S]] = {
    val incomingDependecies = reactiveElement.dependencies.map(_.instance)
    ticket { t => t.bud(Pulse.none, transient = false, incomingDependecies) }
  }
}

class ReactiveElementImpl[S <: Struct](private val graph: ReactiveGraph[S]) extends ReactiveElement[S] {
  private val _instance : Option[Reactive[S]] = None
  private val _bud : Option[S#Spore[Reactive[S]]] = None
  private val _dependencies = scala.collection.mutable.Set[ReactiveElement[S]]()
  graph.registerElement(this)
  override def dependencies  = Set() ++ _dependencies

  override def newDependentElement(): ReactiveElement[S] = {
    val newDependency = new ReactiveElementImpl[S](graph)
    _dependencies += newDependency
    newDependency
  }

  private def instantiate(): Reactive[S] = {
    val incomingDependecies = dependencies.map(_.instance)
    val instance = new ManagedReactive[S](this)
    graph.ticket { t => t.create(incomingDependecies, dynamic = true)(instance)}
  }

  override def hasInstances: Boolean = _instance.isDefined

  override def instance: Reactive[S] = _instance.getOrElse(instantiate())

  override protected[meta] def bud = _bud.getOrElse(graph.createBudForElement(this))
}*/
