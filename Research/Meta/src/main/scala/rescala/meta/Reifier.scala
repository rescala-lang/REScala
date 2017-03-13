package rescala.meta

import rescala.engine.{Engine, TurnSource}
import rescala.graph.{Pulsing, Struct}
import rescala.propagation.Turn
import rescala.reactives.{Evt, _}

trait Reifier[S <: Struct] {
  // External interface that can be used directly
  def reifyEvent[T](eventNode: EventNode[T]) : Event[T, S]
  def reifySignal[A](signalNode: SignalNode[A]) : Signal[A, S]
  def reifyEvt[T](evtNode: EvtEventNode[T]) : Evt[T, S] = reifyEvent(evtNode).asInstanceOf[Evt[T, S]]
  def reifyVar[A](varNode: VarSignalNode[A]) : Var[A, S] = reifySignal(varNode).asInstanceOf[Var[A, S]]

  def logOrApply[T](metaLog: MetaLog[T]): Unit
  def unreify(node : DataFlowNode[_])(implicit ticket : TurnSource[S]): Unit

  // Internal methods to create the corresponding reactive base value
  protected[meta] def createEvt[T]() : Evt[T, S]
  protected[meta] def createVar[A]() : Var[A, S]

  // Internal methods to create reactive nodes with dependencies
  protected[meta] def doReifyEvent[T](eventNode: EventNode[T]) : Event[T, S]
  protected[meta] def doReifySignal[A](signalPointer: SignalNode[A]) : Signal[A, S]
}

class EngineReifier[S <: Struct]()(implicit val engine: Engine[S, Turn[S]]) extends Reifier[S] {
  private val reifiedCache : collection.mutable.Map[DataFlowNode[_], Pulsing[_, S]] = collection.mutable.Map()

  def unreify(node : DataFlowNode[_])(implicit ticket : TurnSource[S]): Unit = {
    for (n <- node.graph.outgoingDependencies(node))
      unreify(n)
    reifiedCache.get(node) match {
      case Some(r) =>
        val savedPulse = ticket { t:Turn[S] => r.pulse(t.makeTicket()) }
        node.graph.savePulse(node, savedPulse)
        r match {
          case e:Event[_, _] => e.disconnect()
          case s:Signal[_, _] => s.disconnect()
        }
      case None => ()
    }
  }

  def logOrApply[T](metaLog: MetaLog[T]): Unit = {
    val graph = metaLog.node.graph
    graph.addLog(metaLog)
    metaLog match {
      case LoggedSet(node, value) if node.deref.get.hasReification => applyLog(graph.popLog())
      case LoggedFire(node, value) if node.deref.get.hasReification => applyLog(graph.popLog())
    }
  }

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

  override def reifyEvent[T](eventNode: EventNode[T]): Event[T, S] = {
      applyLog(eventNode.graph.popLog())
      doReifyEvent(eventNode)
  }

  override def reifySignal[A](signalNode: SignalNode[A]): Signal[A, S] = {
      applyLog(signalNode.graph.popLog())
      doReifySignal(signalNode)
  }

  override protected[meta] def doReifyEvent[T](eventNode: EventNode[T]): Event[T, S] = {
    doReify(eventNode).asInstanceOf[Event[T, S]]
  }

  override protected[meta] def doReifySignal[A](signalNode: SignalNode[A]): Signal[A, S] = {
    doReify(signalNode).asInstanceOf[Signal[A, S]]
  }

  private def doDisconnect[T](node: DataFlowNode[T]): Unit = node match {
    case p: EventNode[_] => p.reify(this).disconnect()
    case p: SignalNode[_] => p.reify(this).disconnect()
  }

  private def doReify[T](node: DataFlowNode[T]): Pulsing[T, S] = {
    val reified = reifiedCache.getOrElse(node, node match {
      case p: ReactiveNode[_] => p.createReification(this)
    }).asInstanceOf[Pulsing[T, S]]
    reifiedCache += node -> reified
    reified
  }
  override protected[meta] def createEvt[T](): engine.Evt[T] = engine.Evt[T]()

  override protected[meta] def createVar[A](): engine.Var[A] = engine.Var.empty[A]
}
