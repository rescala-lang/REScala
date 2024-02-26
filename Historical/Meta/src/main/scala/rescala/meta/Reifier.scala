package reactives.meta

import reactives.core.{CreationTicket, Engine, ReactiV, Struct}

import reactives.reactives.{Evt, _}

import scala.annotation.tailrec
import scala.collection.mutable

case class Reification[S <: Struct](reified: Reactive[S], observers: mutable.Set[(ObserverData[S], Observe[S])])

trait Reifier[S <: Struct] {
  // External interface that can be used directly
  def reifyEvent[T](eventNode: EventNode[T]): Event[T, S]
  def reifySignal[A](signalNode: SignalNode[A]): Signal[A, S]
  def reifyEvt[T](evtNode: EvtEventNode[T]): Evt[T, S]  = reifyEvent(evtNode).asInstanceOf[Evt[T, S]]
  def reifyVar[A](varNode: VarSignalNode[A]): Var[A, S] = reifySignal(varNode).asInstanceOf[Var[A, S]]

  def evaluateNecessaryReification(graph: DataFlowGraph): Unit
  def unreify(node: DataFlowNode[_])(implicit ticket: CreationTicket[S]): Unit

  // Internal methods to create the corresponding reactive base value
  protected[meta] def createEvt[T](): Evt[T, S]
  protected[meta] def createVar[A](): Var[A, S]
  protected[meta] def createVar[A](value: A): Var[A, S]

  // Internal methods to create reactive nodes with dependencies
  protected[meta] def doReifyEvent[T](eventNode: EventNode[T]): Event[T, S]
  protected[meta] def doReifySignal[A](signalPointer: SignalNode[A]): Signal[A, S]
}

class EngineReifier[S <: Struct]()(implicit val engine: Engine[S]) extends Reifier[S] {
  private val reifiedCache: mutable.Map[DataFlowNode[_], Reification[S]]   = collection.mutable.Map()
  private val savedState: mutable.Map[DataFlowNode[_], List[Signal[_, _]]] = collection.mutable.Map()

  private def saveAllStates(node: DataFlowNode[_])(implicit ticket: CreationTicket[S]): Unit = {
    reifiedCache.get(node) match {
      case Some(Reification(reified, _)) =>
        savedState += (node -> Signals.Impl.getStates(reified)._2)
      case None => ()
    }
    for (n <- node.graph.outgoingDataFlow(node))
      saveAllStates(n)
  }

  private def _unreify(node: DataFlowNode[_])(implicit ticket: CreationTicket[S]) = {
    for (n <- node.graph.outgoingDataFlow(node))
      n.unreify(this, ticket)
    reifiedCache.get(node) match {
      case Some(Reification(reified, observers)) =>
        reified match {
          case e: Event[_, _]  => e.disconnect()
          case s: Signal[_, _] => s.disconnect()
        }
        observers.foreach(_._2.remove())
      case None => ()
    }
  }

  def unreify(node: DataFlowNode[_])(implicit ticket: CreationTicket[S]): Unit = {
    saveAllStates(node)
    _unreify(node)
  }

  def evaluateNecessaryReification(graph: DataFlowGraph): Unit = {
    @tailrec
    def findTriggeredObserve(log: List[MetaLog[_]]): Option[(DataFlowNode[_], DataFlowNode[_])] =
      log match {
        case LoggedObserve(DataFlowRef(node), _) :: tail => findTrigger(tail, node) match {
            case Some(t) => Some((node, t))
            case None    => findTriggeredObserve(tail)
          }
        case _ :: tail => findTriggeredObserve(tail)
        case Nil       => None
      }

    @tailrec
    def findTrigger(log: List[MetaLog[_]], observe: DataFlowNode[_]): Option[DataFlowNode[_]] =
      log match {
        case LoggedFire(DataFlowRef(trigger), _) :: tail =>
          if (graph.isDataFlowPossible(trigger, observe)) Some(trigger) else findTrigger(tail, observe)
        case LoggedSet(DataFlowRef(trigger), _) :: tail =>
          if (graph.isDataFlowPossible(trigger, observe)) Some(trigger) else findTrigger(tail, observe)
        case head :: tail => findTrigger(tail, observe)
        case Nil          => None
      }

    val nodeToReevaluate = findTriggeredObserve(graph.log).map(_._2).orElse(
      reifiedCache.filter(_._2.observers.nonEmpty).flatMap(r => findTrigger(graph.log, r._1)).headOption
    )
    if (nodeToReevaluate.isDefined) reifyGraph(graph)
  }

  private def reifyGraph(graph: DataFlowGraph): Unit = {
    val removed = mutable.Queue[MetaLog[_]]()
    graph.log.foreach {
      case LoggedCreate(DataFlowRef(node))     => doReify(node)
      case LoggedDisconnect(DataFlowRef(node)) => doDisconnect(node)
      case f @ LoggedFire(DataFlowRef(node), value) =>
        removed += f
        reifiedCache.getOrElse(node, throw new IllegalArgumentException("Cannot fire a non-reified event!")) match {
          case Reification(e: Evt[Any, S] @unchecked, _) => e.fire(value)
          case other => throw new IllegalStateException(s"expected Reification(Evt) but got $other")
        }
      case s @ LoggedSet(DataFlowRef(node), value) =>
        removed += s
        reifiedCache.getOrElse(node, throw new IllegalArgumentException("Cannot set a non-reified var!")) match {
          case Reification(v: Var[Any, S] @unchecked, _) => v.set(value)
          case other => throw new IllegalStateException(s"expected Reification(Evt) but got $other")
        }
      case LoggedObserve(DataFlowRef(node), od @ ObserverData(onSuccess, onFailure, ticket)) =>
        reifiedCache.getOrElse(
          node,
          throw new IllegalArgumentException("Cannot observe a non-reified reactive node!")
        ) match {
          case Reification(e: Event[Nothing, S] @unchecked, observers) =>
            if (!observers.exists(_._1 == od)) {
              val observe = e.observe(onSuccess, onFailure)(ticket.asInstanceOf[CreationTicket[S]])
              observers += ((od.asInstanceOf[ObserverData[S]], observe))
            }
          case Reification(s: Signal[Nothing, S] @unchecked, observers) =>
            if (!observers.exists(_._1 == od)) {
              val observe = s.observe(onSuccess, onFailure)(ticket.asInstanceOf[CreationTicket[S]])
              observers += ((od.asInstanceOf[ObserverData[S]], observe))
            }
          case _ => throw new IllegalArgumentException("Found unknown log entry type!")
        }
      case _ => ()
    }
    removed.foreach(graph.removeLog)
  }

  override def reifyEvent[T](eventNode: EventNode[T]): Event[T, S] = {
    reifyGraph(eventNode.graph)
    doReifyEvent(eventNode)
  }

  override def reifySignal[A](signalNode: SignalNode[A]): Signal[A, S] = {
    reifyGraph(signalNode.graph)
    doReifySignal(signalNode)
  }

  override protected[meta] def doReifyEvent[T](eventNode: EventNode[T]): Event[T, S] = {
    doReify(eventNode).asInstanceOf[Event[T, S]]
  }

  override protected[meta] def doReifySignal[A](signalNode: SignalNode[A]): Signal[A, S] = {
    doReify(signalNode).asInstanceOf[Signal[A, S]]
  }

  private def doDisconnect[T](node: DataFlowNode[T]): Unit =
    node match {
      case p: EventNode[_]  => doReifyEvent(p).disconnect()
      case p: SignalNode[_] => doReifySignal(p).disconnect()
    }

  private def doReify[T](node: DataFlowNode[T]): ReactiV[T, S] = {
    val reification = reifiedCache.getOrElse(
      node,
      node match {
        case p: ReactiveNode[_] => savedState.get(node) match {
            case Some(state) => Reification(
                Signals.Impl.restoreFrom(state) { p.createReification(this) },
                mutable.Set[(ObserverData[S], Observe[S])]()
              )
            case None => Reification(p.createReification(this), mutable.Set[(ObserverData[S], Observe[S])]())
          }
      }
    )
    reifiedCache += node -> reification
    reification.reified.asInstanceOf[ReactiV[T, S]]
  }

  override protected[meta] def createEvt[T](): engine.Evt[T]         = engine.Evt[T]()
  override protected[meta] def createVar[A](): engine.Var[A]         = engine.Var.empty[A]
  override protected[meta] def createVar[A](value: A): engine.Var[A] = engine.Var(value)

}
