package rescala.extra.simpleprop.invariant

import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import rescala.core
import rescala.core.Initializer.InitValues
import rescala.core.{AccessTicket, Derived, DynamicInitializerLookup, InitialChange, Initializer, Observation, Pulse, ReSource, ReevTicket, Scheduler, Struct}
import rescala.interface.Aliases
import rescala.reactives.InvariantViolationException
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait SimpleStruct extends Struct {
  override type State[V, S <: Struct] = SimpleState[V]
}

class SimpleState[V](ip: InitValues[V]) {

  var value: V = ip.initialValue
  var outgoing: Set[Derived[SimpleStruct]] = Set.empty
  var incoming: Set[ReSource[SimpleStruct]] = Set.empty
  var discovered = false
  var dirty = false
  var done = false
  var invariants: Seq[V => Boolean] = Seq.empty
  var gen: Gen[_] = _

  def reset(): Unit = {
    discovered = false
    dirty = false
    done = false
    value = ip.unchange.unchange(value)
  }

  override def toString: String = s"State(outgoing = $outgoing, discovered = $discovered, dirty = $dirty, done = $done)"
}

class SimpleInitializer(afterCommitObservers: ListBuffer[Observation]) extends Initializer[SimpleStruct] {
  override protected[this] def makeDerivedStructState[V](ip: InitValues[V])
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
    incoming.foreach { dep =>
      dep.state.outgoing += reactive
    }
    reactive.state.incoming = incoming
    reactive.state.discovered = ignitionRequiresReevaluation
    reactive.state.dirty = ignitionRequiresReevaluation
    createdReactives :+= reactive


    val predecessorsDone = incoming.forall(r => !r.state.discovered || r.state.done)
    // requires reev, any predecessor is dirty, but all discovered predecessors are already done
    val requiresReev = incoming.exists(_.state.dirty) && predecessorsDone
    // if discovered, we are mid reevaluation
    val discovered = incoming.exists(_.state.discovered)
    if (discovered && !predecessorsDone) {
      // do nothing, this reactive is reached by normal propagation later
    }
    else if (ignitionRequiresReevaluation || requiresReev) {
      Util.evaluate(reactive, this, afterCommitObservers)
    }
    else if (predecessorsDone) reactive.state.done = true
  }

}


object SimpleScheduler extends DynamicInitializerLookup[SimpleStruct, SimpleInitializer]
  with Scheduler[SimpleStruct]
  with Aliases[SimpleStruct] {

  override def schedulerName: String = "SimpleWithInvariantSupport"

  var idle = true

  override def forceNewTransaction[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R = synchronized {
    if (!idle) throw new IllegalStateException("Scheduler is not reentrant")
    idle = false
    val afterCommitObservers: ListBuffer[Observation] = ListBuffer.empty
    val res = try {
      val creation = new SimpleInitializer(afterCommitObservers)
      withDynamicInitializer(creation) {
        // admission
        val admissionTicket: AdmissionTicket = new AdmissionTicket(creation, initialWrites) {
          override private[rescala] def access(reactive: ReSource): reactive.Value = reactive.state.value
        }
        val admissionResult = admissionPhase(admissionTicket)
        val sources = admissionTicket.initialChanges.values.collect {
          case iv if iv.writeValue(iv.source.state.value, iv.source.state.value = _) => iv.source
        }.toSeq

        creation.drainCreated().foreach(_.state.reset())

        val initial = sources.flatMap { s =>
          s.state.dirty = true
          s.state.done = true
          s.state.discovered = true
          s.state.outgoing
        }

        initial.foreach { r =>
          r.state.dirty = true
        }

        // propagation
        val sorted = Util.toposort(initial)
        Util.evaluateAll(sorted, creation, afterCommitObservers).foreach(_.state.reset())
        // evaluate everything that was created, but not accessed, and requires ignition
        val created = creation.drainCreated()
        Util.evaluateAll(created, creation, afterCommitObservers).foreach(_.state.reset())
        assert(creation.drainCreated().isEmpty)

        Util.evaluateInvariants(created ++ sorted, initialWrites)

        //cleanup
        initial.foreach(_.state.reset())
        created.foreach(_.state.reset())
        sources.foreach(_.state.reset())
        sorted.foreach(_.state.reset())


        //wrapup
        if (admissionTicket.wrapUp != null) admissionTicket.wrapUp(creation.accessTicket())
        admissionResult
      }
    }
    finally {
      idle = true
    }
    afterCommitObservers.foreach(_.execute())
    res
  }

  override private[rescala] def singleReadValueOnce[A](reactive: Signal[A]): A = {
    val id = reactive.innerDerived
    id.interpret(id.state.value)
  }

  def specify[T](inv: Seq[T => Boolean], signal: Signal[T]): Unit = {
    signal.state.invariants = inv.map(inv => ((invp: Pulse[T]) => inv(invp.get)))
  }

  implicit class SignalWithInvariants[T](val signal: Signal[T]) extends AnyVal {

    def specify(inv: (T => Boolean) *): Unit = {
      SimpleScheduler.this.specify(inv, signal)
    }

    def setValueGenerator(gen: Gen[T]): Unit = {
      this.signal.state.gen = gen
    }

    def test(): Unit = {
      if (this.signal.state.gen != null) {
        0 to 100 foreach {
          _ => forceValues((this.signal, Pulse.Value(this.signal.state.gen.pureApply(Gen.Parameters.default, Seed.random()))))
        }
      } else {
        //Find generator for each branch. select a value for each. force transaction
        throw new NotImplementedException()
      }
    }



    private def forceValues(changes: (Signal[A], A) forSome { type A } *): Unit = {
      val asReSource = changes.foldLeft(Set.empty[core.ReSource[SimpleStruct]]) {case (acc, (source, _)) => acc + source}

      forceNewTransaction(asReSource, {
        admissionTicket =>
          changes.foreach {
            change =>
              admissionTicket.recordChange(new InitialChange[SimpleStruct] {
                override val source: core.ReSource[SimpleStruct] = signal.innerDerived

                override def writeValue(b: source.Value, v: source.Value => Unit): Boolean = {
                  val casted = change._2.asInstanceOf[source.Value]
                  if(casted != b) {
                    v(casted)
                    return true
                  }
                  false
                }
              })
          }
      })
      Util.evaluateInvariants(asReSource.toSeq, asReSource)
    }
  }
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
  def evaluateAll(evaluatees: Seq[Derived[SimpleStruct]], creation: SimpleInitializer, afterCommitObservers: ListBuffer[Observation]): Seq[Derived[SimpleStruct]] = {
    // first one where evaluation detects glitch
    val glitched = evaluatees.reverseIterator.find { r =>
      if (r.state.done) false
      else if (r.state.dirty) {
        Util.evaluate(r, creation, afterCommitObservers)
      }
      else false
    }
    glitched match {
      case None => evaluatees
      case Some(reactive) =>
        val evaluateNext = evaluatees.filterNot(_.state.done) ++ creation.drainCreated()
        evaluateNext.foreach(_.state.discovered = false)
        evaluateAll(Util.toposort(evaluateNext), creation, afterCommitObservers)
    }
  }

  def evaluate(reactive: Derived[SimpleStruct], creationTicket: SimpleInitializer, afterCommitObservers: ListBuffer[Observation]): Boolean = {
    var potentialGlitch = false
    val dt = new ReevTicket[reactive.Value, SimpleStruct](creationTicket, reactive.state.value) {
      override def dynamicAccess(input: ReSource[SimpleStruct]): input.Value = {
        if (input.state.discovered && !input.state.done) {
          potentialGlitch = true
        }
        input.state.value
      }

      override def staticAccess(input: ReSource[SimpleStruct]): input.Value = input.state.value
    }
    val reev = reactive.reevaluate(dt)
    reev.getDependencies().foreach { newDeps =>
      val incoming = reactive.state.incoming
      reactive.state.incoming = newDeps
      val added = newDeps diff incoming
      val removed = incoming diff newDeps
      added.foreach { input =>
        input.state.outgoing = input.state.outgoing + reactive
      }
      removed.foreach { input =>
        input.state.outgoing = input.state.outgoing - reactive
      }
    }

    if (potentialGlitch) true else {
      if (reev.propagate) reactive.state.outgoing.foreach(_.state.dirty = true)
      reev.forValue(reactive.state.value = _)
      reev.forEffect(o => afterCommitObservers.append(o))
      reactive.state.done = true
      false
    }

  }

  def evaluateInvariants(reactives: Seq[ReSource[SimpleStruct]], initialWrites: Set[ReSource[SimpleStruct]]): Unit = {
    for {
      reactive <- reactives
      inv <- reactive.state.invariants
      if !inv(reactive.state.value)
    } {
      throw new InvariantViolationException(new IllegalArgumentException(s"${reactive.state.value}"), reactive, Util.getCausalErrorChains(reactive, initialWrites)) // TODO: why is no assertionerror thrown?
    }
  }

  def getCausalErrorChains(errorNode: ReSource[SimpleStruct], initialWrites: Set[ReSource[SimpleStruct]]): Seq[Seq[ReSource[SimpleStruct]]] = {
    import scala.collection.mutable.ListBuffer

    val initialNames = initialWrites.map(_.name)

    def traverse(node: ReSource[SimpleStruct], path: Seq[ReSource[SimpleStruct]]): Seq[Seq[ReSource[SimpleStruct]]] = {
      val paths = new ListBuffer[Seq[ReSource[SimpleStruct]]]()
      for (incoming <- node.state.incoming) {
        val incName = incoming.name
        if (initialNames.contains(incName)) {
          paths += path :+ incoming
        }
        else {
          paths ++= traverse(incoming, path :+ incoming)
        }
      }
      paths.toList
    }

    traverse(errorNode, Seq(errorNode))
  }
}
