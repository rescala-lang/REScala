package rescala.simpleprop

import rescala.core.Initializer.InitValues
import rescala.core.{CreationTicket, Initializer, ReSource, Reactive, ReevTicket, Scheduler, Struct}
import rescala.interface.Aliases

import scala.collection.mutable.ArrayBuffer

trait SimpleStruct extends Struct {
  override type State[V, S <: Struct] = SimpleState[V]
}

class SimpleState[V](ip: InitValues[V]) {

  var value: V = ip.initialValue
  var outgoing: Set[Reactive[SimpleStruct]] = Set.empty
  var discovered = false
  var dirty = false
  var done = false
  def reset(): Unit = {
    discovered = false
    dirty = false
    done = false
    value = ip.unchange.unchange(value)
  }

  override def toString: String = s"State(outgoing = $outgoing, discovered = $discovered, dirty = $dirty, done = $done)"
}

object SimpleCreation extends Initializer[SimpleStruct] {
  override protected[this] def makeDerivedStructState[V](ip: InitValues[V], creationTicket: CreationTicket[SimpleStruct])
  : SimpleState[V] = new SimpleState[V](ip)

  override protected[this] def ignite(reactive: Reactive[SimpleStruct],
                                      incoming: Set[ReSource[SimpleStruct]],
                                      ignitionRequiresReevaluation: Boolean)
  : Unit = {
    incoming.foreach { dep =>
      dep.state.outgoing += reactive
    }

    if (ignitionRequiresReevaluation) {
      Util.evaluate(reactive, incoming)
      reactive.state.reset()
    }
  }

}


object SimpleScheduler extends Scheduler[SimpleStruct] with Aliases[SimpleStruct] {

  override def schedulerName: String = "Simple"

  override def executeTurn[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R = synchronized {
    val initial: ArrayBuffer[Reactive] = ArrayBuffer[Reactive]()
    val sorted: ArrayBuffer[Reactive] = ArrayBuffer[Reactive]()

    // admission
    val admissionTicket = new AdmissionTicket(SimpleCreation, initialWrites) {
      override def access[A](reactive: Signal[A]): reactive.Value = reactive.state.value
    }
    val admissionResult = admissionPhase(admissionTicket)
    val sources = admissionTicket.initialChanges.values.collect {
      case iv if iv.writeValue(iv.source.state.value, iv.source.state.value = _) => iv.source
    }.toSeq
    sources.foreach{ s =>
      s.state.dirty = true
      s.state.done = true
      s.state.outgoing.foreach { r =>
        r.state.dirty = true
        initial += r
      }
    }

    // propagation
    @scala.annotation.tailrec
    def propagation(): Unit = {
      Util.toposort(initial, sorted)
      println(s"sort order determinded to be $sorted")
      val propagationIterator = sorted.reverseIterator
      // first one where evaluation detects glitch
      val glitched = propagationIterator.find { r =>
        println(s"looking at $r with ${r.state}")
        if (r.state.done) false
        else if (r.state.dirty) {
          println(s"evaluating $r")
          Util.evaluate(r, Set.empty)
        }
        else false
      }
      glitched match {
        case None =>
        case Some(reactive) =>
          initial.foreach(_.state.discovered = false)
          sorted.foreach(_.state.discovered = false)
          sorted.clear()
          propagation()
      }
    }
    propagation()

    //cleanup
    initial.foreach(_.state.reset)
    sources.foreach(_.state.reset)
    sorted.foreach(_.state.reset)

    initial.clear()
    sorted.clear()


    //wrapup
    if (admissionTicket.wrapUp != null) admissionTicket.wrapUp(new WrapUpTicket {
      override private[rescala] def access(reactive: ReSource): reactive.Value = reactive.state.value
    })
    admissionResult
  }
  override private[rescala] def singleReadValueOnce[A](reactive: Signal[A]): A = reactive.interpret(reactive.state.value)
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

  def evaluate(reactive: Reactive[SimpleStruct], incoming: Set[ReSource[SimpleStruct]]): Boolean = {
    var potentialGlitch = false
    val dt = new ReevTicket[reactive.Value, SimpleStruct](SimpleCreation, reactive.state.value) {
      override def dynamicAccess(input: ReSource[SimpleStruct]): input.Value = {
        if (input.state.discovered && !input.state.done)
          potentialGlitch = true
        input.state.value
      }
      override def staticAccess(input: ReSource[SimpleStruct]): input.Value = input.state.value
    }
    val reev = reactive.reevaluate(dt)
    reev.getDependencies().foreach {
      _.foreach { input =>
        input.state.outgoing = input.state.outgoing + reactive
      }
    }

    if (potentialGlitch) true else {
      if (reev.propagate) reactive.state.outgoing.foreach(_.state.dirty = true)
      reev.forValue(reactive.state.value = _)
      reev.forEffect(_ ())
      reactive.state.done = true
      false
    }
  }
}
