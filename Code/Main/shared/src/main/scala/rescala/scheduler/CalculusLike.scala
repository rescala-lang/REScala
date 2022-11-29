/** This implementation tries to mirror the formalization
  * of the corresponding paper as closely as possible
  */
package rescala.scheduler

import rescala.core.Core

trait CalculusLike extends Core {

  type State[V] = StoreValue[V]

  /** The formalization uses a per device store mapping reactives to their
    * inputs, values, and operator.
    * The operator is already handled by the common implementation, so we keep the value and inputs.
    * The store mapping does not exist as a single object, but instead each reactive has this state.
    */
  class StoreValue[V](var value: V) {
    var inputs: Set[ReSource]     = Set.empty
    override def toString: String = s""
  }

  /** The main task of the initializer is to handle creation of reactives,
    * especially during an ongoing propagation.
    * The formalization does not support this, to keep the complexity of the proofs in check.
    */
  final class SimpleCreation() extends Initializer {
    override protected[this] def makeDerivedStructState[V](initialValue: V): StoreValue[V] =
      new StoreValue[V](initialValue)

    override protected[this] def register(reactive: ReSource): Unit = FScheduler.allReactives += reactive

    override protected[this] def initialize(
        reactive: Derived,
        incoming: Set[ReSource],
        needsReevaluation: Boolean
    ): Unit = {
      println(s"creating $reactive $needsReevaluation")
      println(incoming)

      reactive.state.inputs = incoming

      if (needsReevaluation || requiresReev(reactive)) {
        println(s"creation evaluation $reactive")
        // evaluate immediately to support some higher order + creation nonsense
        Reevaluate.evaluate(reactive, _ => true, FTransaction(this))
        ()
      }
    }

    def requiresReev(reSource: ReSource): Boolean = {
      if (FScheduler.currentPropagation == null) false
      else
        FScheduler.currentPropagation.isReady(reSource) &&
        FScheduler.currentPropagation.isOutdated(reSource)
    }

  }

  case class FTransaction(override val initializer: Initializer) extends Transaction {
    override private[rescala] def access(reactive: ReSource): reactive.Value = reactive.state.value
    override def observe(obs: Observation): Unit = obs.execute()
  }

  object FScheduler
      extends SchedulerImpl[FTransaction] {

    override def schedulerName: String = "FormalizationLike"

    var allReactives                    = Set.empty[ReSource]
    var currentPropagation: Propagation = null

    var idle = true

    /** this corresponds very roughly to the fire rule.
      * The initial writes contains the reactives which change (only one for fire),
      * and teh admission phase updates their values to the fired values (μ(r).val ← v)
      */
    override def forceNewTransaction[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R =
      synchronized {
        // some broken user code may start a new transaction during an ongoing one
        // this is not supported by this propagation algorithm,
        // and is detected here because it leads to weird behaviour otherwise
        if (!idle) throw new IllegalStateException("Scheduler is not reentrant")
        idle = false
        try {
          println(s"\nexecuting turn from $initialWrites")
          val transaction = FTransaction(new SimpleCreation())
          withDynamicInitializer(transaction) {
            // admission
            val admissionTicket = new AdmissionTicket(transaction, initialWrites)

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
            val propagation = Propagation(active = sources, processed = sources, allReactives, transaction)
            println(s"starting propagation $propagation")
            val result = propagation.run()
            println(s"done activate")

            // wrapup, this is for a rarely used rescala features, where transactions can
            // do some cleanup when they complete. Not supported in the formalization
            if (admissionTicket.wrapUp != null) admissionTicket.wrapUp(transaction)

            // commit cleans up some internal state
            result.commit()

            // transactions return whatever the user code did
            admissionResult
          }
        } finally {
          currentPropagation = null
          idle = true
        }
      }
    override private[rescala] def singleReadValueOnce[A](reactive: ReadAs[A]): A =
      reactive.read(reactive.state.value)
  }

  case class Propagation(
      active: Set[ReSource],
      processed: Set[ReSource],
      knownReactives: Set[ReSource],
      transaction: FTransaction
  ) {

    // resets the state of all reactives back to whatever it should be after propagation
    // this is used primarily to reset all events back to no value
    def commit(): Unit =
      active.foreach { a =>
        a.state.value = a.commit(a.state.value)
      }

    def run(): Propagation = {
      // make this available, as we may need it to create new reactives
      FScheduler.currentPropagation = this
      // println("propagating:")
      // println(s"active: $active\nprocessed: $processed\n" +
      //        s"ready: $ready\noutdated: $outdated\nall: $allReactives\n" +
      //        s"unprocessed: $unprocessed")

      // we may process more than just the known initial reactives,
      // because REScala allows the creation of new reactives during propagation.
      // their correctness is ensured by the creation method
      if (knownReactives == processed) this // snapshots are handled elsewhere
      else {                                // reevaluate or skip

        // if there is anything to skip, we just skip all of them at once
        val toSkip = ready -- outdated
        if (toSkip.nonEmpty) {
          // println(s"skipping: $toSkip")
          Propagation(active, processed ++ toSkip, knownReactives, transaction).run()
        }
        // if there is nothing to skip, we continue with reevaluation
        else {

          // rule does not specify which reactive to select
          val candidates = ready.intersect(outdated)
          val r          = candidates.head
          // Reevaluate r. Sources are handled differently from derived reactives,
          // as they have no attached operation in REScala
          // in the calculus they just execute the empty operation
          println(s"reevaluating $r")
          val oldRstring = r.state.inputs
          val (evaluated, propagate) = r match {
            case r: Derived =>
              Reevaluate.evaluate(r, isReady, transaction)
            case _ => (true, false)
          }

          val newReactives = FScheduler.allReactives -- knownReactives
          if (newReactives.nonEmpty) {
            println(s"created reactives $newReactives")
          }

          if (evaluated) {
            val nextActive = if (propagate) active + r else active
            Propagation(nextActive, processed + r, knownReactives, transaction).run()
          } else {
            println(s"redoing \n$oldRstring\nto\n${r.state.inputs}")
            Propagation(active, processed, knownReactives, transaction).run()
          }
        }
      }
    }

    /** helper for better inspection */
    lazy val unprocessed: Set[ReSource] = knownReactives -- processed

    /** Compute the set of all ready reactives. Logic is identical to the paper. */
    lazy val ready: Set[ReSource] = {
      unprocessed.filter { r =>
        // intersect with all known reactives
        // as there may be new inputs that were created during this propagation
        // which we always consider as processed
        isReady(r)
      }
    }

    def isReady(r: ReSource): Boolean = {
      r.state.inputs.intersect(knownReactives).subsetOf(processed + r)
    }

    /** Compute outdated reactives. Logic is identical to the paper. */
    lazy val outdated: Set[ReSource] = {
      knownReactives.filter(isOutdated)
    }
    def isOutdated(r: ReSource): Boolean = {
      r.state.inputs.exists(active.contains)
    }
  }

  object Reevaluate {
    def evaluate(
        reactive: Derived,
        dynamicOk: ReSource => Boolean,
        transaction: FTransaction
    ): (Boolean, Boolean) = {
      val dt = new ReevTicket[reactive.Value](
        transaction,
        reactive.state.value,
        new AccessHandler {
          override def dynamicAccess(input: ReSource): input.Value = input.state.value
          override def staticAccess(input: ReSource): input.Value  = input.state.value
        }
      )

      val reev = reactive.reevaluate(dt)

      def finishReevaluation() = {
        // good, store value (write rule)
        reev.forValue(reactive.state.value = _)
        // and execute observers (not covered in the formalization)
        // real schedulers may want to postpone these to the the commit
        reev.forEffect(_.execute())
        (true, reev.activate)
      }

      reev.inputs() match {
        case None => // static reactive
          finishReevaluation()
        case Some(inputs) => // dynamic reactive
          reactive.state.inputs = inputs
          if (dynamicOk(reactive)) finishReevaluation()
          else (false, reev.activate)

      }
    }
  }
}
