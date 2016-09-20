package rescala.meta

import rescala.graph.{SimpleStruct, Struct}
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

object SynchronousReifier extends Reifier[SimpleStruct] {
  import rescala.engines.CommonEngines.synchron

  private val reifiedCache : collection.mutable.Map[ReactiveNode, Any] = collection.mutable.Map()

  // TODO: Find a way to prevent instanceOf-cast
  private def applyLog(log : List[MetaLog]): Unit = {
    log.foreach {
      case LoggedFire(node, value) => reifiedCache.getOrElse(node, throw new IllegalArgumentException("Cannot fire a non-reified event!")) match {
        case e : Evt[_, _] => e.asInstanceOf[Evt[Any, SimpleStruct]].fire(value)
      }
      case LoggedSet(node, value) => reifiedCache.getOrElse(node, throw new IllegalArgumentException("Cannot set a non-reified var!")) match {
        case v: Var[_, _] => v.asInstanceOf[Var[Any, SimpleStruct]].set(value)
      }
    }
  }

  override protected[meta] def reifyEvt[T](evtPointer: EvtEventPointer[T]): Evt[T, SimpleStruct] = reifyEvent(evtPointer).asInstanceOf[Evt[T, SimpleStruct]]

  override protected[meta] def reifyVar[A](varPointer: VarSignalPointer[A]): Var[A, SimpleStruct] = reifySignal(varPointer).asInstanceOf[Var[A, SimpleStruct]]

  override protected[meta] def reifyEvent[T](eventPointer: EventPointer[T]): Event[T, SimpleStruct] = eventPointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val reified = doReify(eventPointer).asInstanceOf[Event[T, SimpleStruct]]
      applyLog(node.graph.popLog())
      reified
  }

  override protected[meta] def reifySignal[A](signalPointer: SignalPointer[A]): Signal[A, SimpleStruct] = signalPointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val reified = doReify(signalPointer).asInstanceOf[Signal[A, SimpleStruct]]
      applyLog(node.graph.popLog())
      reified
  }

  override protected[meta] def reifyObserve[T](observePointer: ObservePointer[T]): Observe[SimpleStruct] = observePointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val reified = doReify(observePointer).asInstanceOf[Observe[SimpleStruct]]
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
  override def createEvt[T](evtPointer: EvtEventPointer[T]) = synchron.Evt[T]()

  override def createVar[A](varPointer: VarSignalPointer[A]) = synchron.Var.empty[A]
}
