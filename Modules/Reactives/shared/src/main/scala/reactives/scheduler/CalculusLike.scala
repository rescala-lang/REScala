/** This implementation tries to mirror the formalization
  * of the corresponding paper as closely as possible
  */
package reactives.scheduler

import reactives.core.ReSource.of
import reactives.core.{AccessHandler, AdmissionTicket, Derived, DynamicTicket, Initializer, Observation, ReSource, ReadAs, ReevTicket, SchedulerWithDynamicScope, Transaction}

object CalculusLike {

  type State[V] = StoreValue[V]

  /** The formalization uses a per device store mapping reactives to their
    * inputs, values, and operator.
    * The operator is already handled by the common implementation, so we keep the value and inputs.
    * The store mapping does not exist as a single object, but instead each reactive has this state.
    */
  class StoreValue[V](var value: V) {
    var inputs: Set[ReSource.of[State]] = Set.empty
    override def toString: String       = s""
  }

  /** The main task of the initializer is to handle creation of reactives,
    * especially during an ongoing propagation.
    * The formalization does not support this, to keep the complexity of the proofs in check.
    */
  final class SimpleCreation() extends Initializer[State] {

    override protected def makeDerivedStructState[V](initialValue: V): StoreValue[V] =
      new StoreValue[V](initialValue)

    override protected def register[V](
        reactive: ReSource.of[State],
        inputs: Set[ReSource.of[State]],
        initialValue: V
    ): Unit = {
      super.register(reactive, inputs, initialValue)
      FScheduler.allReactives += reactive
    }

    override protected def initialize(
        reactive: Derived.of[State],
        incoming: Set[ReSource.of[State]],
        needsReevaluation: Boolean
    ): Unit = {
      println(s"creating $reactive $needsReevaluation")
      println(incoming)

      reactive.state.inputs = incoming

      if needsReevaluation || requiresReev(reactive) then {
        println(s"creation evaluation $reactive")
        // evaluate immediately to support some higher order + creation nonsense
        Reevaluate.evaluate(reactive, _ => true, FTransaction(this))
        ()
      }
    }

    def requiresReev(reSource: ReSource.of[State]): Boolean = {
      if FScheduler.currentPropagation == null then false
      else
        FScheduler.currentPropagation.nn.isReady(reSource) &&
        FScheduler.currentPropagation.nn.isOutdated(reSource)
    }

  }

  case class FTransaction(override val initializer: Initializer[CalculusLike.this.State])
      extends Transaction[State] {
    override private[reactives] def access(reactive: ReSource.of[State]): reactive.Value = reactive.state.value
    override def observe(obs: Observation): Unit                                         = obs.execute()

    override def preconditionTicket: DynamicTicket[State] = new DynamicTicket[State](this):
      override private[reactives] def collectDynamic(reactive: ReSource.of[State]) = access(reactive)
      override private[reactives] def collectStatic(reactive: ReSource.of[State])  = access(reactive)
  }

  object FScheduler
      extends SchedulerWithDynamicScope[State, FTransaction] {

    override def schedulerName: String = "FormalizationLike"

    var allReactives                           = Set.empty[ReSource.of[State]]
    var currentPropagation: Propagation | Null = null

    var idle = true

    /** this corresponds very roughly to the fire rule.
      * The initial writes contains the reactives which change (only one for fire),
      * and teh admission phase updates their values to the fired values (μ(r).val ← v)
      */
    override def forceNewTransaction[R](
        initialWrites: Set[ReSource.of[State]],
        admissionPhase: AdmissionTicket[State] => R
    ): R = {
      synchronized {
        // some broken user code may start a new transaction during an ongoing one
        // this is not supported by this propagation algorithm,
        // and is detected here because it leads to weird behaviour otherwise
        if !idle then throw new IllegalStateException("Scheduler is not reentrant")
        idle = false
        try {
          println(s"\nexecuting turn from $initialWrites")
          val transaction = FTransaction(new SimpleCreation())
          dynamicScope.withDynamicInitializer(transaction) {
            // admission
            val admissionTicket = new AdmissionTicket[State](transaction, initialWrites)

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
            if admissionTicket.wrapUp != null then admissionTicket.wrapUp.nn(transaction)

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
    }
    override private[reactives] def singleReadValueOnce[A](reactive: ReadAs.of[State, A]): A =
      reactive.read(reactive.state.value)
  }

  case class Propagation(
      active: Set[ReSource.of[State]],
      processed: Set[ReSource.of[State]],
      knownReactives: Set[ReSource.of[State]],
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
      if knownReactives == processed then this // snapshots are handled elsewhere
      else {                                   // reevaluate or skip

        // if there is anything to skip, we just skip all of them at once
        val toSkip = ready -- outdated
        if toSkip.nonEmpty then {
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
          if newReactives.nonEmpty then {
            println(s"created reactives $newReactives")
          }

          if evaluated then {
            val nextActive = if propagate then active + r else active
            Propagation(nextActive, processed + r, knownReactives, transaction).run()
          } else {
            println(s"redoing \n$oldRstring\nto\n${r.state.inputs}")
            Propagation(active, processed, knownReactives, transaction).run()
          }
        }
      }
    }

    /** helper for better inspection */
    lazy val unprocessed: Set[ReSource.of[State]] = knownReactives -- processed

    /** Compute the set of all ready reactives. Logic is identical to the paper. */
    lazy val ready: Set[ReSource.of[State]] = {
      unprocessed.filter { r =>
        // intersect with all known reactives
        // as there may be new inputs that were created during this propagation
        // which we always consider as processed
        isReady(r)
      }
    }

    def isReady(r: ReSource.of[State]): Boolean = {
      r.state.inputs.intersect(knownReactives).subsetOf(processed + r)
    }

    /** Compute outdated reactives. Logic is identical to the paper. */
    lazy val outdated: Set[ReSource.of[State]] = {
      knownReactives.filter(isOutdated)
    }
    def isOutdated(r: ReSource.of[State]): Boolean = {
      r.state.inputs.exists(active.contains)
    }
  }

  object Reevaluate {
    def evaluate(
        reactive: Derived.of[State],
        dynamicOk: ReSource.of[State] => Boolean,
        transaction: FTransaction
    ): (Boolean, Boolean) = {
      val dt = new ReevTicket[State, reactive.Value](
        transaction,
        reactive.state.value,
        new AccessHandler[State] {
          override def dynamicAccess(input: ReSource.of[State]): input.Value = input.state.value
          override def staticAccess(input: ReSource.of[State]): input.Value  = input.state.value
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
          if dynamicOk(reactive) then finishReevaluation()
          else (false, reev.activate)

      }
    }
  }
}
