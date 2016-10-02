package rescala.meta

import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import rescala.reactives.{Evt, _}

import scala.language.higherKinds

trait Reifier[S <: Struct] {
  protected[meta] def createEvt[T](evtPointer: EvtEventPointer[T]) : Evt[T, S]
  protected[meta] def createVar[A](varPointer: VarSignalPointer[A]) : Var[A, S]

  protected[meta] def reifyEvt[T](evtPointer: EvtEventPointer[T]) : Evt[T, S]
  protected[meta] def reifyVar[A](varPointer: VarSignalPointer[A]) : Var[A, S]

  protected[meta] def reifyEvent[T](eventPointer: EventPointer[T]) : Event[T, S]
  protected[meta] def reifySignal[A](signalPointer: SignalPointer[A]) : Signal[A, S]
  protected[meta] def reifyObserve[A](observePointer: ObservePointer[A]) : Observe[S]
}

class EngineReifier[S <: Struct]()(implicit val engine: Engine[S, Turn[S]]) extends Reifier[S] {

  private val reifiedCache : collection.mutable.Map[ReactiveNode[_], Any] = collection.mutable.Map()

  // TODO: Find a way to prevent instanceOf-cast
  private def applyLog(log : List[MetaLog[_]]): Unit = {
    log.foreach {
      case LoggedFire(node, value) => reifiedCache.getOrElse(node, throw new IllegalArgumentException("Cannot fire a non-reified event!")) match {
        case e : Evt[_, _] => e.asInstanceOf[Evt[Any, S]].fire(value)
      }
      case LoggedSet(node, value) => reifiedCache.getOrElse(node, throw new IllegalArgumentException("Cannot set a non-reified var!")) match {
        case v: Var[_, _] => v.asInstanceOf[Var[Any, S]].set(value)
      }
    }
  }

  override protected[meta] def reifyEvt[T](evtPointer: EvtEventPointer[T]): Evt[T, S] = reifyEvent(evtPointer).asInstanceOf[Evt[T, S]]

  override protected[meta] def reifyVar[A](varPointer: VarSignalPointer[A]): Var[A, S] = reifySignal(varPointer).asInstanceOf[Var[A, S]]

  override protected[meta] def reifyEvent[T](eventPointer: EventPointer[T]): Event[T, S] = eventPointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val reified = doReify(eventPointer).asInstanceOf[Event[T, S]]
      applyLog(node.graph.popLog())
      reified
  }

  override protected[meta] def reifySignal[A](signalPointer: SignalPointer[A]): Signal[A, S] = signalPointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val reified = doReify(signalPointer).asInstanceOf[Signal[A, S]]
      applyLog(node.graph.popLog())
      reified
  }

  override protected[meta] def reifyObserve[T](observePointer: ObservePointer[T]): Observe[S] = observePointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val reified = doReify(observePointer).asInstanceOf[Observe[S]]
      applyLog(node.graph.popLog())
      reified
  }

  private def doReify[T](pointer: MetaPointer[T]): Any = pointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val reified = reifiedCache.getOrElse(node, pointer match {
        case p: ReactivePointer[_] => p.createReification(this)
        case p: ObservePointer[_] => p.createReification(this)
      })
      reifiedCache += node -> reified
      reified
  }
  override def createEvt[T](evtPointer: EvtEventPointer[T]) = engine.Evt[T]()

  override def createVar[A](varPointer: VarSignalPointer[A]) = engine.Var.empty[A]
}
