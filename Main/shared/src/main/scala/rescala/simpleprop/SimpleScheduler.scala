package rescala.simpleprop

import rescala.core.Initializer.InitValues
import rescala.core.{CreationTicket, Initializer, ReSource, Reactive, ReevTicket, Scheduler, DynamicInitializerLookup, Struct}
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

class SimpleCreation(creations: ArrayBuffer[Reactive[SimpleStruct]]) extends Initializer[SimpleStruct] {
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
      creations += reactive
    }
  }

}


object SimpleScheduler extends DynamicInitializerLookup[SimpleStruct, SimpleCreation]
                       with Scheduler[SimpleStruct]
                       with Aliases[SimpleStruct] {

  override def schedulerName: String = "Simple"

  override def executeTurn[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R = synchronized {
    val creations: ArrayBuffer[Reactive] = ArrayBuffer[Reactive]()
    val creation = new SimpleCreation(creations)
    withDynamicInitializer(creation) {
      // admission
      val admissionTicket = new AdmissionTicket(creation, initialWrites) {
        override def access[A](reactive: Signal[A]): reactive.Value = reactive.state.value
      }
      val admissionResult = admissionPhase(admissionTicket)
      val sources = admissionTicket.initialChanges.values.collect {
        case iv if iv.writeValue(iv.source.state.value, iv.source.state.value = _) => iv.source
      }.toSeq

      val initial = sources.flatMap { s =>
        s.state.dirty = true
        s.state.done = true
        s.state.outgoing
      }

      initial.foreach { r =>
        r.state.dirty = true
      }

      // propagation
      val sorted = Util.toposort(initial)
      Util.evaluateAll(sorted, creation)

      //cleanup
      initial.foreach(_.state.reset)
      sources.foreach(_.state.reset)
      sorted.foreach(_.state.reset)


      //wrapup
      if (admissionTicket.wrapUp != null) admissionTicket.wrapUp(new WrapUpTicket {
        override private[rescala] def access(reactive: ReSource): reactive.Value = reactive.state.value
      })
      admissionResult
    }
  }
  override private[rescala] def singleReadValueOnce[A](reactive: Signal[A]): A = reactive.interpret(reactive.state.value)
}


object Util {
  def toposort(rem: Seq[Reactive[SimpleStruct]]): Seq[Reactive[SimpleStruct]] = {
    val sorted = ArrayBuffer[Reactive[SimpleStruct]]()
    def _toposort(rem: Reactive[SimpleStruct]): Unit = {
      if (rem.state.discovered) ()
      else {
        rem.state.discovered = true
        rem.state.outgoing.foreach(_toposort)
        sorted += rem
      }
    }

    rem.foreach(_toposort)
    sorted
  }

  @scala.annotation.tailrec
  def evaluateAll(evaluatees: Seq[Reactive[SimpleStruct]], creation: SimpleCreation): Unit = {
    println(s"sort order determinded to be $evaluatees")
    // first one where evaluation detects glitch
    val glitched = evaluatees.reverseIterator.find { r =>
      println(s"looking at $r with ${r.state}")
      if (r.state.done) false
      else if (r.state.dirty) {
        println(s"evaluating $r")
        Util.evaluate(r, Set.empty, creation)
      }
      else false
    }
    glitched match {
      case None =>
      case Some(reactive) =>
        evaluatees.foreach(_.state.discovered = false)
        evaluateAll(Util.toposort(evaluatees), creation)
    }
  }

  def evaluate(reactive: Reactive[SimpleStruct], incoming: Set[ReSource[SimpleStruct]], creationTicket: SimpleCreation): Boolean = {
    var potentialGlitch = false
    val dt = new ReevTicket[reactive.Value, SimpleStruct](creationTicket, reactive.state.value) {
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
