package rescala.simpleprop

import rescala.core.Initializer.InitValues
import rescala.core.{AccessTicket, CreationTicket, DynamicInitializerLookup, Initializer, ReSource, Derived, ReevTicket, Scheduler, Struct}
import rescala.interface.Aliases

import scala.collection.mutable.ArrayBuffer

trait SimpleStruct extends Struct {
  override type State[V, S <: Struct] = SimpleState[V]
}

class SimpleState[V](ip: InitValues[V]) {

  var value: V                             = ip.initialValue
  var outgoing: Set[Derived[SimpleStruct]] = Set.empty
  var discovered                           = false
  var dirty                                = false
  var done                                 = false
  def reset(): Unit = {
    discovered = false
    dirty = false
    done = false
    value = ip.unchange.unchange(value)
  }

  override def toString: String = s"State(outgoing = $outgoing, discovered = $discovered, dirty = $dirty, done = $done)"
}

class SimpleCreation() extends Initializer[SimpleStruct] {
  override protected[this] def makeDerivedStructState[V](ip: InitValues[V], creationTicket: CreationTicket[SimpleStruct])
  : SimpleState[V] = new SimpleState[V](ip)

  private var createdReactives: Seq[Derived[SimpleStruct]] = Seq.empty


  override def accessTicket(): AccessTicket[SimpleStruct] = new AccessTicket[SimpleStruct] {
    override private[rescala] def access(reactive: ReSource[SimpleStruct]): reactive.Value = reactive.state.value
  }


  def drainCreated(): Seq[Derived[SimpleStruct]] = {
    val tmp = createdReactives
    createdReactives = Seq.empty
    tmp
  }

  override protected[this] def ignite(reactive: Derived[SimpleStruct],
                                      incoming: Set[ReSource[SimpleStruct]],
                                      ignitionRequiresReevaluation: Boolean)
  : Unit = {
    println(s"creating $reactive")
    println(incoming)
    incoming.foreach { dep =>
      dep.state.outgoing += reactive
    }
    reactive.state.discovered = true
    createdReactives :+= reactive

    val dirty = incoming.filter(_.state.dirty)
    // require reev, if no
    val requiresReev = dirty.nonEmpty && dirty.forall(_.state.done)
    println(requiresReev)

    if (ignitionRequiresReevaluation || requiresReev) {
      println(s"creation evaluation $reactive")
      // evaluate immediately to support some higher order + creation nonsense
      Util.evaluate(reactive, incoming, this)
    }
    else if (incoming.forall(dep => !dep.state.discovered || dep.state.done)) reactive.state.done = true
  }

}


object SimpleScheduler extends DynamicInitializerLookup[SimpleStruct, SimpleCreation]
                       with Scheduler[SimpleStruct]
                       with Aliases[SimpleStruct] {

  override def schedulerName: String = "Simple"

  var idle = true

  override def forceNewTransaction[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R = synchronized {
    if (!idle) throw new IllegalStateException("Scheduler is not reentrant")
    idle = false
    try {
      println(s"\nexecuting turn from $initialWrites")
      val creation = new SimpleCreation()
      withDynamicInitializer(creation) {
        // admission
        val admissionTicket = new AdmissionTicket(creation, initialWrites) {
          override private[rescala] def access(reactive: ReSource): reactive.Value = reactive.state.value
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
        println(s"initial $initial")
        val sorted = Util.toposort(initial)
        println(s"sorted $sorted")
        Util.evaluateAll(sorted, creation).foreach(_.state.reset())
        val created = creation.drainCreated()
        Util.evaluateAll(created, creation).foreach(_.state.reset())
        assert(creation.drainCreated().isEmpty)

        //cleanup
        initial.foreach(_.state.reset)
        created.foreach(_.state.reset)
        sources.foreach(_.state.reset)
        sorted.foreach(_.state.reset)


        //wrapup
        if (admissionTicket.wrapUp != null) admissionTicket.wrapUp(creation.accessTicket())
        admissionResult
      }
    }
    finally {
      idle = true
    }
  }
  override private[rescala] def singleReadValueOnce[A](reactive: Signal[A]): A = reactive.interpret(reactive.state.value)
}


object Util {
  def toposort(rem: Seq[Derived[SimpleStruct]]): Seq[Derived[SimpleStruct]] = {
    val sorted = ArrayBuffer[Derived[SimpleStruct]]()
    def _toposort(rem: Derived[SimpleStruct]): Unit = {
      if (rem.state.discovered) ()
      else {
        rem.state.discovered = true
        rem.state.outgoing.foreach(_toposort)
        sorted += rem
      }
    }

    rem.foreach(_toposort)
    // need toSeq for 2.13, where Seq is immutable
    sorted.toSeq
  }

  @scala.annotation.tailrec
  def evaluateAll(evaluatees: Seq[Derived[SimpleStruct]], creation: SimpleCreation): Seq[Derived[SimpleStruct]] = {
    println(s"evaluating $evaluatees")
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
      case None => evaluatees
      case Some(reactive) =>
        println(s"glitched: $reactive")
        val evaluateNext = evaluatees ++ creation.drainCreated()
        evaluateNext.foreach(_.state.discovered = false)
        evaluateAll(Util.toposort(evaluateNext), creation)
    }
  }

  def evaluate(reactive: Derived[SimpleStruct], incoming: Set[ReSource[SimpleStruct]], creationTicket: SimpleCreation): Boolean = {
    var potentialGlitch = false
    val dt = new ReevTicket[reactive.Value, SimpleStruct](creationTicket, reactive.state.value) {
      override def dynamicAccess(input: ReSource[SimpleStruct]): input.Value = {
        if (input.state.discovered && !input.state.done) {
          println(s"glitch: $reactive accessed $input")
          potentialGlitch = true
        }
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
      reev.forEffect(_.execute())
      reactive.state.done = true
      false
    }
  }
}
