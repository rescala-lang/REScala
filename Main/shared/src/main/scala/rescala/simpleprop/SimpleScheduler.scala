package rescala.simpleprop

import rescala.core.Initializer.Param
import rescala.core.{Initializer, ReSource, ReSourciV, Reactive, ReevTicket, Scheduler, Struct}

import scala.collection.mutable.ArrayBuffer

trait SimpleStruct extends Struct {
  override type State[P, S <: Struct] = SimpleState[P]
}

class SimpleState[V](var value: V, transient: Option[V]) {
  var outgoing: Set[Reactive[SimpleStruct]] = Set.empty
  var discovered = false
  var dirty = false
  def reset(): Unit = {
    discovered = false
    dirty = false
    transient.foreach(value = _)
  }
}

object SimpleCreation extends Initializer[SimpleStruct] {
  override protected[this] def makeDerivedStructState[P](valuePersistency: Param[P]): SimpleState[P] =
    new SimpleState[P](valuePersistency.initialValue, if (valuePersistency.isTransient) Some(valuePersistency.initialValue) else None)
  override protected[this] def ignite(reactive: Reactive[SimpleStruct], incoming: Set[ReSource[SimpleStruct]], ignitionRequiresReevaluation: Boolean): Unit = {

    incoming.foreach { dep =>
      dep.state.outgoing += reactive
    }

    if (ignitionRequiresReevaluation) {
      Util.evaluate(reactive, incoming)
    }
  }

}


class SimpleScheduler extends Scheduler[SimpleStruct] {
  override private[rescala] def executeTurn[R](initialWrites: Traversable[ReSource], admissionPhase: AdmissionTicket => R) = {

    // admission
    val admissionTicket = new AdmissionTicket(SimpleCreation) {
      override def access[A](reactive: ReSourciV[A, SimpleStruct]): A = reactive.state.value
    }
    val admissionResult = admissionPhase(admissionTicket)
    val initials = admissionTicket.initialChanges.valuesIterator.collect {
      case ic if ic.accept(ic.source.state.value) =>
        ic.source.state.value = ic.value
        ic.source
    }.toSeq
    def initialOutgoing = initials.iterator.flatMap(_.state.outgoing)
    initialOutgoing.foreach(_.state.dirty = true)

    // propagation
    val order = Util.toposort(initialOutgoing.toIterable)
    order.reverseIterator.foreach(r => if(r.state.dirty) Util.evaluate(r, Set.empty))

    //cleanup
    initials.foreach(_.state.reset)
    order.foreach(_.state.reset)

    //wrapup
    if (admissionTicket.wrapUp != null) ???
    admissionResult
  }
  override private[rescala] def singleNow[A](reactive: ReSourciV[A, SimpleStruct]) = reactive.state.value
  override private[rescala] def create[T](f: Creation => T) = f(SimpleCreation)
}


object Util {
  def toposort(rem: Iterable[Reactive[SimpleStruct]]): ArrayBuffer[Reactive[SimpleStruct]] = {
    val res = ArrayBuffer[Reactive[SimpleStruct]]()
    def _toposort(rem: Reactive[SimpleStruct]): Unit = {
      if (rem.state.discovered) ()
      else {
        rem.state.discovered = true
        rem.state.outgoing.foreach(_toposort)
        res += rem
      }
    }
    rem.foreach(_toposort)
    res
  }

  val dt = new ReevTicket[Nothing, SimpleStruct](SimpleCreation) {
    override def dynamicAccess[A](reactive: ReSourciV[A, SimpleStruct]): A = ???
    override def staticAccess[A](reactive: ReSourciV[A, SimpleStruct]): A = reactive.state.value
  }

  def evaluate(reactive: Reactive[SimpleStruct], incoming: Set[ReSource[SimpleStruct]]): Unit = {
    val reev = reactive.reevaluate(dt.reset(), reactive.state.value)
    if (reev.propagate) reactive.state.outgoing.foreach(_.state.dirty = true)
    if (reev.getDependencies().isDefined) ???
    reev.forValue(reactive.state.value = _)
    reev.forEffect(_())
  }
}
