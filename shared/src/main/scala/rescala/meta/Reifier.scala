package rescala.meta

import rescala.graph.{SimpleStruct, Struct}
import rescala.reactives.{Evt, _}

import scala.language.higherKinds

trait Reifier[S <: Struct, SL[+X, Z <: Struct], EV[+X, Z <: Struct] , VAR[X, Z <: Struct], EVT[X, Z <: Struct]] {
  protected[meta] def createEvt[T](evtPointer: EvtEventPointer[T]) : EVT[T, S]
  protected[meta] def createVar[A](varPointer: VarSignalPointer[A]) : VAR[A, S]

  protected[meta] def reifyEvent[T](eventPointer: MetaEventPointer[T]) : EV[T, S]
  protected[meta] def reifySignal[A](eventPointer: MetaSignalPointer[A]) : SL[A, S]
}

object SynchronousReifier extends Reifier[SimpleStruct, Signal, Event, Var, Evt] {
  import rescala.engines.CommonEngines.synchron

  private val reifiedSignalCache : collection.mutable.Map[ReactiveNode, Signal[_, SimpleStruct]] = collection.mutable.Map()
  private val reifiedEventCache : collection.mutable.Map[ReactiveNode, Event[_, SimpleStruct]] = collection.mutable.Map()

  // TODO: Find a way to prevent instanceOf-cast
  private def applyLog(log : List[MetaLog]): Unit = {
    log.foreach {
      case LoggedFire(node, value) => reifiedEventCache.getOrElse(node, throw new IllegalArgumentException("Cannot fire a non-reified event!")) match {
        case e : Evt[_, SimpleStruct] => e.asInstanceOf[Evt[Any, SimpleStruct]].fire(value)
      }
      case LoggedSet(node, value) => reifiedSignalCache.getOrElse(node, throw new IllegalArgumentException("Cannot set a non-reified var!")) match {
        case v: Var[_, SimpleStruct] => v.asInstanceOf[Var[Any, SimpleStruct]].set(value)
      }
    }
  }

  override protected[meta] def reifyEvent[T](eventPointer: MetaEventPointer[T]): Event[T, SimpleStruct] = eventPointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val event = reifiedEventCache.getOrElse(node, eventPointer.createReification(this))
      reifiedEventCache += node -> event
      applyLog(node.graph.popLog())
      event.asInstanceOf[Event[T, SimpleStruct]]
  }

  // TODO: Find a way to prevent instanceOf-cast
  override protected[meta] def reifySignal[A](signalPointer: MetaSignalPointer[A]): Signal[A, SimpleStruct] = signalPointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val event = reifiedSignalCache.getOrElse(node, signalPointer.createReification(this))
      reifiedSignalCache += node -> event
      applyLog(node.graph.popLog())
      event.asInstanceOf[Signal[A, SimpleStruct]]
  }


  override def createEvt[T](evtPointer: EvtEventPointer[T]) = synchron.Evt[T]()

  override def createVar[A](varPointer: VarSignalPointer[A]) = synchron.Var.empty[A]


}
