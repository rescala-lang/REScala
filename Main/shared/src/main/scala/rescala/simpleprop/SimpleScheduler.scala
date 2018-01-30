package rescala.simpleprop

import rescala.core.Initializer.Param
import rescala.core.{Initializer, ReSource, Reactive, ReevTicket, Scheduler, Struct}

import scala.collection.mutable.ArrayBuffer

trait SimpleStruct extends Struct {
  override type State[P, S <: Struct, N] = SimpleState[P, N]
}

class SimpleState[V, N](var value: V,
                        transient: Option[V]) {
  var outgoing: Set[Reactive[SimpleStruct]] = Set.empty
  var notification: N = _
  var discovered = false
  var dirty = false
  def reset(): Unit = {
    discovered = false
    dirty = false
    transient.foreach(value = _)
    notification = null.asInstanceOf[N]
  }
}

object SimpleCreation extends Initializer[SimpleStruct] {
  override protected[this] def makeDerivedStructState[P, N](valuePersistency: Param[P]): SimpleState[P, N] =
    new SimpleState[P, N](valuePersistency.initialValue, if (valuePersistency.isTransient) Some(valuePersistency.initialValue) else None)
  override protected[this] def ignite(reactive: Reactive[SimpleStruct], incoming: Set[ReSource[SimpleStruct]], ignitionRequiresReevaluation: Boolean): Unit = {

    incoming.foreach { dep =>
      dep.state.outgoing += reactive
    }

    if (ignitionRequiresReevaluation) {
      Util.evaluate(reactive, incoming)
    }
  }

}


object SimpleScheduler extends Scheduler[SimpleStruct] {
  val initial: ArrayBuffer[Reactive] = ArrayBuffer[Reactive]()
  val sorted: ArrayBuffer[Reactive] = ArrayBuffer[Reactive]()
  var idle = true
  override private[rescala] def executeTurn[R](initialWrites: Traversable[ReSource], admissionPhase: AdmissionTicket => R) = synchronized {
    require(idle, "simple scheduler is not reentrant")
    idle = false
    try {
      // admission
      val admissionTicket = new AdmissionTicket(SimpleCreation) {
        override def access[A](reactive: Signal[A]): A = reactive.state.value.get
      }
      val admissionResult = admissionPhase(admissionTicket)
      val sources = admissionTicket.initialChanges.collect {
        case ic if ic.accept(ic.source.state.value) =>
          if (ic.source.isInstanceOf[Evt[_]])
            ic.source.state.notification = ic.value.asInstanceOf[ic.source.Notification]
          else ic.source.state.value = ic.value
          ic.source
      }.toSeq
      sources.foreach(_.state.outgoing.foreach(initial += _))
      initial.foreach(_.state.dirty = true)

      // propagation
      Util.toposort(initial, sorted)
      initial.clear()
      sorted.reverseIterator.foreach(r => if (r.state.dirty) Util.evaluate(r, Set.empty))

      //cleanup
      sources.foreach(_.state.reset)
      sorted.foreach(_.state.reset)

      sorted.clear()


      //wrapup
      if (admissionTicket.wrapUp != null) admissionTicket.wrapUp(new WrapUpTicket {
        override private[rescala] def access[A](reactive: ReSource) = reactive.state.notification
      })
      admissionResult
    } finally idle = true
  }
  override private[rescala] def singleNow[A](reactive: Signal[A]) = reactive.state.value.get
  override private[rescala] def create[T](f: Creation => T) = f(SimpleCreation)
}


object Util {
  def toposort(rem: ArrayBuffer[Reactive[SimpleStruct]], sorted: ArrayBuffer[Reactive[SimpleStruct]]): Unit = {
    def _toposort(rem: Reactive[SimpleStruct]): Unit = {
      if (rem.state.discovered) ()
      else {
        rem.state.discovered = true
        rem.state.outgoing.foreach(_toposort)
        sorted += rem
      }
    }

    rem.foreach(_toposort)
  }

  val dt = new ReevTicket(SimpleCreation, null) {
    override def dynamicAccess[A](reactive: ReSource[SimpleStruct]) = ???
    override def staticAccess[A](reactive: ReSource[SimpleStruct]) = (reactive.state.value, reactive.state.notification)
  }

  def evaluate(reactive: Reactive[SimpleStruct], incoming: Set[ReSource[SimpleStruct]]): Unit = {
    val reev = reactive.reevaluate(dt.reset(reactive.state.value))
    if (reev.propagate) reactive.state.outgoing.foreach(_.state.dirty = true)
    if (reev.getDependencies().isDefined) ???
    reev.forValue(reactive.state.value = _)
    reev.forNotification(reactive.state.notification = _)
    reev.forEffect(_ ())
  }
}
