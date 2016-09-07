package rescala.meta

import rescala.graph.{SimpleStruct, Struct}
import rescala.reactives.{Evt, _}

import scala.language.higherKinds

trait Reifier[S <: Struct, SL[+X, Z <: Struct] <: SignalLike[X, Z, SL, EV], EV[+X, Z <: Struct] <: EventLike[X, Z, SL, EV]] {
  def reifyEvent[T](eventPointer: MetaEventPointer[T]) : EV[T, S]

  def reifySignal[A](eventPointer: MetaSignalPointer[A]) : SL[A, S]
}

object SynchronousReifier extends Reifier[SimpleStruct, Signal, Event] {
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

  // TODO: Find a way to prevent instanceOf-cast
  override def reifyEvent[T](eventPointer: MetaEventPointer[T]): Event[T, SimpleStruct] = eventPointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val event = reifiedEventCache.getOrElse(node, eventPointer match {
      case EvtEventPointer(n) => synchron.Evt()
      case AndEventPointer(n, base, other, merger) => base.reify(this).and(other.reify(this))(merger)
      case OrEventPointer(n, base, other) => base.reify(this) || other.reify(this)
      case fep@FilteredEventPointer(n, base, pred) => base.reify(this).filter(pred.asInstanceOf[T => Boolean])
      case ChangeEventPointer(n, base) => base.reify(this).change
      case ChangedEventPointer(n, base) => base.reify(this).changed
      case ExceptEventPointer(n, base, other) => base.reify(this) \ other.reify(this)
      //case FlatMappedEventPointer(n, base, f) => base.reify(this).flatMap(f)
      //case MappedEventPointer(n, base, mapping) => base.reify(this).map(mapping)
      //case FlattenedEventPointer(n, base) => base.reify(this).flatten
      })
      reifiedEventCache += node -> event
      applyLog(node.graph.popLog())
      event.asInstanceOf[Event[T, SimpleStruct]]
  }

  // TODO: Find a way to prevent instanceOf-cast
  override def reifySignal[A](signalPointer: MetaSignalPointer[A]): Signal[A, SimpleStruct] = signalPointer.node match {
    case None => throw new IllegalArgumentException("Cannot reify null pointer!")
    case Some(node) =>
      val event = reifiedSignalCache.getOrElse(node, signalPointer match {
        case VarSignalPointer(n) => synchron.Var(null)
        case DelayedSignalPointer(n, base, delay) => base.reify(this).delay(delay)
        //case FlattenedSignalPointer(n, base) => base.reify(this).flatten
        //case FoldedSignalPointer(n, base, init, fold) => base.reify(this).fold(init)(fold)
        //case MappedSignalPointer(n, base, mapping) => base.reify(this).map(mapping)
        case SnapshotSignalPointer(n, base, s) => base.reify(this).snapshot(s.reify(this))
        case SwitchOnceSignalPointer(n, base, origS, newS) => base.reify(this).switchOnce(origS.reify(this), newS.reify(this))
        case SwitchToSignalPointer(n, base, original) => base.reify(this).switchTo(original.reify(this))
        case ToggledSignalPointer(n, base, a, b) => base.reify(this).toggle(a.reify(this), b.reify(this))
      })
      reifiedSignalCache += node -> event
      applyLog(node.graph.popLog())
      event.asInstanceOf[Signal[A, SimpleStruct]]
  }
}
