/** This implementation tries to mirror the formalization
  * of the corresponding paper as closely as possible */
package rescala.research

import rescala.core.Initializer.InitValues
import rescala.core.{AccessTicket, CreationTicket, Derived, DynamicInitializerLookup, Initializer, ReSource, ReevTicket, Scheduler, Struct}
import rescala.interface.Aliases
import rescala.research.FScheduler.allReactives


trait FStruct extends Struct {
  override type State[V, S <: Struct] = StoreValue[V]
}

/** The formalization uses a per device store mapping reactives to their
  * inputs, values, and operator.
  * The operator is already handled by the common implementation, so we keep the value and inputs.
  * The store mapping does not exist as a single object, but instead each reactive has this state. */
class StoreValue[V](ip: InitValues[V]) {

  var value : V                      = ip.initialValue
  var inputs: Set[ReSource[FStruct]] = Set.empty

  override def toString: String = s""
}

/** The main task of the initializer is to handle creation of reactives,
  * especially during an ongoing propagation.
  * The formalization does not support this, to keep the complexity of the proofs in check. */
class SimpleCreation() extends Initializer[FStruct] {
  override protected[this] def makeDerivedStructState[V](ip: InitValues[V], creationTicket: CreationTicket[FStruct])
  : StoreValue[V] = new StoreValue[V](ip)



  override def accessTicket(): AccessTicket[FStruct] = new AccessTicket[FStruct] {
    override private[rescala] def access(reactive: ReSource[FStruct]): reactive.Value = reactive.state.value
  }


  override protected[this] def register(reactive: ReSource[FStruct]): Unit = allReactives += reactive

  override protected[this] def ignite(reactive: Derived[FStruct],
                                      incoming: Set[ReSource[FStruct]],
                                      ignitionRequiresReevaluation: Boolean)
  : Unit = {
    println(s"creating $reactive")
    println(incoming)

    reactive.state.inputs = incoming


    if (ignitionRequiresReevaluation) {
      println(s"creation evaluation $reactive")
      // evaluate immediately to support some higher order + creation nonsense
      Reevaluate.evaluate(reactive, r => true, this)
    }
  }

}


object FScheduler extends DynamicInitializerLookup[FStruct, SimpleCreation]
                  with Scheduler[FStruct]
                  with Aliases[FStruct] {

  override def schedulerName: String = "FormalizationLike"

  var allReactives = Set.empty[ReSource]

  var idle = true

  /** this corresponds very roughly to the fire rule.
    * The initial writes contains the reactives which change (only one for fire),
    * and teh admission phase updates their values to the fired values (μ(r).val ← v) */
  override def forceNewTransaction[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R = synchronized {
    // some broken user code may start a new transaction during an ongoing one
    // this is not supported by this propagation algorithm,
    // and is detected here because it leads to weird behaviour otherwise
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

        // collect the fired values
        val admissionResult = admissionPhase(admissionTicket)
        // write all fired values to the store (r.writeValue)
        // we collect those where the write is propagated
        // (non propagation is not supported by the formalization, but is used for filter events,
        // and signals that do not change their values
        val sources = admissionTicket.initialChanges.values.collect {
          case r if r.writeValue(r.source.state.value, r.source.state.value = _) => r.source
        }.toSet

        println(s"sources: $sources")

        // start a new propagation changes in REScala always have to happen as a side effect
        val propagation = Propagation(active = sources, processed = sources, creation)
        println(s"starting propagation $propagation")
        propagation.run()
        println(s"done propagating")


        // wrapup, this is for a rarely used rescala features, where transactions can
        // do some cleanup when they complete. Not supported in the formalization
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


case class Propagation(active: Set[ReSource[FStruct]], processed: Set[ReSource[FStruct]], creationTicket: SimpleCreation) {
  def run(): Unit = {
    //println("propagating:")
    //println(s"active: $active\nprocessed: $processed\n" +
    //        s"ready: $ready\noutdated: $outdated\nall: $allReactives\n" +
    //        s"unprocessed: $unprocessed")
    if (processed == allReactives) () // commit and be done, snapshots are handled elsewhere
    else { // reevaluate or skip

      // if there is anything to skip, we just skip all of them at once
      val toSkip = ready -- outdated
      if (toSkip.nonEmpty) {
        println(s"skipping: $toSkip")
        Propagation(active, processed ++ toSkip, creationTicket).run()
      }
      // if there is nothing to skip, we continue with reevaluation
      else {

        // rule does not specify which reactive to select
        val candidates = ready.intersect(outdated)
        val r = candidates.head
        // Reevaluate r. Sources are handled differently from derived reactives,
        // as they have no attached operation in REScala
        // in the calculus they just execute the empty operation
        println(s"reevaluating $r")
        val evaluated = r match {
          case r: Derived[FStruct] => Reevaluate.evaluate(r, r => r.state.inputs.subsetOf(processed + r), creationTicket)
          case _             => true
        }
        if (evaluated) Propagation(active + r, processed + r, creationTicket).run
        else Propagation(active, processed, creationTicket).run()
      }
    }
  }

  /** helper for better inspection */
  lazy val unprocessed: Set[FScheduler.ReSource] = allReactives -- processed

  /** Compute the set of all ready reactives. Logic is identical to the paper. */
  lazy val ready: Set[ReSource[FStruct]] = {
    unprocessed.filter{r =>
      val processedAndI = processed + r
      r.state.inputs.subsetOf(processedAndI)
    }
  }

  /** Compute outdated reactives. Logic is identical to the paper. */
  lazy val outdated: Set[ReSource[FStruct]] = {
    allReactives.filter(r => r.state.inputs.exists(active.contains))
  }
}

object Reevaluate {
  def evaluate(reactive: Derived[FStruct], dynamicOk: ReSource[FStruct] => Boolean, creationTicket: SimpleCreation): Boolean = {
    val dt = new ReevTicket[reactive.Value, FStruct](creationTicket, reactive.state.value) {
      override def dynamicAccess(input: ReSource[FStruct]): input.Value = {
        input.state.value
      }
      override def staticAccess(input: ReSource[FStruct]): input.Value = input.state.value
    }

    val reev = reactive.reevaluate(dt)

    def finishReevaluation() = {
      // good, store value (write rule)
      reev.forValue(reactive.state.value = _)
      // and execute observers (not covered in the formalization)
      // real schedulers may want to postpone these to the the commit
      reev.forEffect(_.execute())
      true
    }

    reev.getDependencies() match {
      case None         => // static reactive
        finishReevaluation()
      case Some(inputs) => //dynamic reactive
        reactive.state.inputs = inputs
        if (dynamicOk(reactive)) finishReevaluation()
        else false

    }
  }
}
