package rescala.simpleprop

import rescala.core.Initializer.InitValues
import rescala.core.{Initializer, ReSource, Reactive, ReevTicket, Scheduler, Struct}

import scala.collection.mutable.ArrayBuffer

trait SimpleStruct extends Struct {
  override type State[V, S <: Struct] = SimpleState[V]
}

class SimpleState[V](ip: InitValues[V]) {

  var value: V = ip.initialValue
  var outgoing: Set[Reactive[SimpleStruct]] = Set.empty
  var discovered = false
  var dirty = false
  def reset(): Unit = {
    discovered = false
    dirty = false
    value = ip.unchange.unchange(value)
  }
}

object SimpleCreation extends Initializer[SimpleStruct] {
  override protected[this] def makeDerivedStructState[V](ip: InitValues[V]): SimpleState[V] =
    new SimpleState[V](ip)
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

  var idle = true
  override private[rescala] def executeTurn[R](initialWrites: Traversable[ReSource], admissionPhase: AdmissionTicket => R) = synchronized {
    require(idle, "simple scheduler is not reentrant")
    idle = false
    try {
      val initial: ArrayBuffer[Reactive] = ArrayBuffer[Reactive]()
      val sorted: ArrayBuffer[Reactive] = ArrayBuffer[Reactive]()
      // admission
      val admissionTicket = new AdmissionTicket(SimpleCreation) {
        override def access[A](reactive: Signal[A]) = reactive.state.value
      }
      val admissionResult = admissionPhase(admissionTicket)
      val sources = admissionTicket.initialChanges.collect {
        case iv if iv.writeValue(iv.source.state.value, iv.source.state.value = _) => iv.source
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
        override private[rescala] def access(reactive: ReSource) = reactive.state.value
      })
      admissionResult
    } finally idle = true
  }
  override private[rescala] def singleReadValueOnce[A](reactive: Signal[A]) = reactive.interpret(reactive.state.value)
  override private[rescala] def creationDynamicLookup[T](f: Creation => T) = f(SimpleCreation)
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

  def evaluate(reactive: Reactive[SimpleStruct], incoming: Set[ReSource[SimpleStruct]]): Unit = {
    val dt = new ReevTicket[reactive.Value, SimpleStruct](SimpleCreation, reactive.state.value) {
      override def dynamicAccess(reactive: ReSource[SimpleStruct]) = ???
      override def staticAccess(reactive: ReSource[SimpleStruct]) = reactive.state.value
    }
    val reev = reactive.reevaluate(dt)
    if (reev.propagate) reactive.state.outgoing.foreach(_.state.dirty = true)
    if (reev.getDependencies().isDefined) ???
    reev.forValue(reactive.state.value = _)
    reev.forEffect(_ ())
  }
}
