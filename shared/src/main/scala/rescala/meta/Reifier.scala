package rescala.meta

import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import rescala.reactives.{Evt, _}

trait Reifier[S <: Struct] {
  // External interface that can be used directly
  def reifyEvent[T](eventNode: EventNode[T]) : Event[T, S]
  def reifySignal[A](signalNode: SignalNode[A]) : Signal[A, S]
  def reifyObserve[T](observeNode: ObserveNode[T]) : Observe[S]
  def reifyEvt[T](evtNode: EvtEventNode[T]) : Evt[T, S]
  def reifyVar[A](varNode: VarSignalNode[A]) : Var[A, S]
  def hasReification(node: DataFlowNode[_]): Boolean

  // Internal methods to create the corresponding reactive base value
  protected[meta] def createEvt[T]() : Evt[T, S]
  protected[meta] def createVar[A]() : Var[A, S]

  // Internal methods to create reactive nodes with dependencies
  protected[meta] def doReifyEvent[T](eventNode: EventNode[T]) : Event[T, S]
  protected[meta] def doReifySignal[A](signalPointer: SignalNode[A]) : Signal[A, S]
  protected[meta] def doReifyObserve[T](observePointer: ObserveNode[T]) : Observe[S]
}

class EngineReifier[S <: Struct]()(implicit val engine: Engine[S, Turn[S]]) extends Reifier[S] {

  private val reifiedCache : collection.mutable.Map[DataFlowNode[_], Any] = collection.mutable.Map()

  private def applyLog(log : List[MetaLog[_]]): Unit = {
    log.foreach {
      case LoggedCreate(DataFlowRef(node)) => doReify(node)
      case LoggedDisconnect(DataFlowRef(node)) => doDisconnect(node)
      case LoggedFire(DataFlowRef(node), value) => reifiedCache.getOrElse(node, throw new IllegalArgumentException("Cannot fire a non-reified event!")) match {
        case e : Evt[_, _] => e.asInstanceOf[Evt[Any, S]].fire(value)
      }
      case LoggedSet(DataFlowRef(node), value) => reifiedCache.getOrElse(node, throw new IllegalArgumentException("Cannot set a non-reified var!")) match {
        case v: Var[_, _] => v.asInstanceOf[Var[Any, S]].set(value)
      }
      case _ => ()
    }
  }

  override def reifyEvt[T](evtNode: EvtEventNode[T]): Evt[T, S] = reifyEvent(evtNode).asInstanceOf[Evt[T, S]]

  override def reifyVar[A](varNode: VarSignalNode[A]): Var[A, S] = reifySignal(varNode).asInstanceOf[Var[A, S]]

  override def reifyEvent[T](eventNode: EventNode[T]): Event[T, S] = {
      applyLog(eventNode.graph.popLog())
      doReifyEvent(eventNode)
  }

  override def reifySignal[A](signalNode: SignalNode[A]): Signal[A, S] = {
      applyLog(signalNode.graph.popLog())
      doReifySignal(signalNode)
  }

  override def reifyObserve[T](observeNode: ObserveNode[T]): Observe[S] = {
      applyLog(observeNode.graph.popLog())
      doReifyObserve(observeNode)
  }

  override def hasReification(node: DataFlowNode[_]): Boolean = reifiedCache.contains(node)

  override protected[meta] def doReifyEvent[T](eventNode: EventNode[T]): Event[T, S] = {
    doReify(eventNode).asInstanceOf[Event[T, S]]
  }

  override protected[meta] def doReifySignal[A](signalNode: SignalNode[A]): Signal[A, S] = {
    doReify(signalNode).asInstanceOf[Signal[A, S]]
  }

  override protected[meta] def doReifyObserve[T](observeNode: ObserveNode[T]): Observe[S] = {
    doReify(observeNode).asInstanceOf[Observe[S]]

  }

  private def doDisconnect[T](node: DataFlowNode[T]): Unit = node match {
    case p: EventNode[_] => p.reify(this).disconnect()
    case p: SignalNode[_] => p.reify(this).disconnect()
    case p: ObserveNode[_] => p.reify(this).remove()
  }

  private def doReify[T](node: DataFlowNode[T]): Any = {
    val reified = reifiedCache.getOrElse(node, node match {
      case p: ReactiveNode[_] => p.createReification(this)
      case p: ObserveNode[_] => p.createReification(this)
    })
    node._hasReification = true
    reifiedCache += node -> reified
    reified
  }
  override protected[meta] def createEvt[T]() = engine.Evt[T]()

  override protected[meta] def createVar[A]() = engine.Var.empty[A]
}
