package rescala.scheduler

import rescala.core.{
  AccessHandler, AdmissionTicket, Initializer, Observation, ReSource, ReadAs, ReevTicket, Scheduler, SchedulerImpl,
  Transaction
}
import rescala.structure.Observe

import scala.annotation.nowarn
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object TopbundleImpl extends TopoBundle {
  override def makeDerivedStructStateBundle[V](ip: V): TopoState[V] = new TopoState[V](ip)
  override type State[V] = TopoState[V]
}

trait TopoBundle {
  type State[V] <: TopoState[V]

  type Derived  = rescala.core.Derived.of[State]
  type ReSource = rescala.core.ReSource.of[State]

  class TopoState[V](var value: V) {

    var outgoing: Set[Derived]  = Set.empty
    var incoming: Set[ReSource] = Set.empty
    var discovered              = false
    var dirty                   = false
    var done                    = false
    def reset(v: V): Unit = {
      discovered = false
      dirty = false
      done = false
      value = v
    }

    override def toString: String =
      s"State(outgoing = $outgoing, discovered = $discovered, dirty = $dirty, done = $done)"
  }

  def makeDerivedStructStateBundle[V](ip: V): State[V]

  class TopoInitializer(afterCommitObservers: ListBuffer[Observation]) extends Initializer[State] {

    override protected[this] def makeDerivedStructState[V](initialValue: V): State[V] =
      makeDerivedStructStateBundle(initialValue)

    private var createdReactives: Seq[Derived] = Seq.empty

    def drainCreated(): Seq[Derived] = {
      val tmp = createdReactives
      createdReactives = Seq.empty
      tmp
    }

    override protected[this] def initialize(
        reactive: Derived,
        incoming: Set[ReSource],
        needsReevaluation: Boolean
    ): Unit = {
      incoming.foreach { dep =>
        dep.state.outgoing += reactive
      }
      reactive.state.incoming = incoming
      reactive.state.discovered = needsReevaluation
      reactive.state.dirty = needsReevaluation
      createdReactives :+= reactive

      val predecessorsDone = incoming.forall(r => !r.state.discovered || r.state.done)
      // requires reev, any predecessor is dirty, but all discovered predecessors are already done
      val requiresReev = incoming.exists(_.state.dirty) && predecessorsDone
      // if discovered, we are mid reevaluation
      val discovered = incoming.exists(_.state.discovered)
      if (discovered && !predecessorsDone) {
        // do nothing, this reactive is reached by normal propagation later
      } else if (needsReevaluation || requiresReev) {
        Util.evaluate(reactive, TopoTransaction(this), afterCommitObservers)
        ()
      } else if (predecessorsDone) reactive.state.done = true
    }

    def observe(obs: Observation): Unit = afterCommitObservers.append(obs)

  }

  case class TopoTransaction(override val initializer: TopoInitializer) extends Transaction[State] {

    override private[rescala] def access(reactive: ReSource): reactive.Value = reactive.state.value
    override def observe(obs: Observation): Unit                             = initializer.observe(obs)
  }

  object TopoScheduler extends TopoSchedulerInterface

  trait TopoSchedulerInterface extends SchedulerImpl[State, TopoTransaction] {

    override def schedulerName: String = "Simple"

    var idle = true

    def reset(r: ReSource) = r.state.reset(r.commit(r.state.value))

    def beforeCleanupHook(@nowarn all: Seq[ReSource], @nowarn initialWrites: Set[ReSource]): Unit = ()

    override def forceNewTransaction[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket[State] => R): R =
      synchronized {
        if (!idle) throw new IllegalStateException("Scheduler is not reentrant")
        idle = false
        val afterCommitObservers: ListBuffer[Observation] = ListBuffer.empty
        val res =
          try {
            val creation    = new TopoInitializer(afterCommitObservers)
            val transaction = TopoTransaction(creation)
            withDynamicInitializer(transaction) {
              // admission
              val admissionTicket: AdmissionTicket[State] = new AdmissionTicket[State](transaction, initialWrites)
              val admissionResult                         = admissionPhase(admissionTicket)
              val sources = admissionTicket.initialChanges.values.collect {
                case iv if iv.writeValue(iv.source.state.value, iv.source.state.value = _) => iv.source
              }.toSeq

              creation.drainCreated().foreach(reset)

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
              Util.evaluateAll(sorted, transaction, afterCommitObservers).foreach(reset)
              // evaluate everything that was created, but not accessed, and requires ignition
              val created = creation.drainCreated()
              Util.evaluateAll(created, transaction, afterCommitObservers).foreach(reset)
              assert(creation.drainCreated().isEmpty)

              beforeCleanupHook(created ++ sorted ++ initialWrites, initialWrites)

              // cleanup
              initial.foreach(reset)
              created.foreach(reset)
              sources.foreach(reset)
              sorted.foreach(reset)

              // wrapup
              if (admissionTicket.wrapUp != null) admissionTicket.wrapUp(transaction)
              admissionResult
            }
          } finally {
            idle = true
          }
        afterCommitObservers.foreach(_.execute())
        res
      }

    override private[rescala] def singleReadValueOnce[A](reactive: ReadAs.of[State, A]): A = {
      reactive.read(reactive.state.value)
    }
  }

  object Util {
    def toposort(rem: Seq[Derived]): Seq[Derived] = {
      val sorted = ArrayBuffer[Derived]()

      def _toposort(rem: Derived): Unit = {
        if (rem.state.discovered) ()
        else {
          rem.state.discovered = true
          rem.state.outgoing.foreach(_toposort)
          sorted += rem
          ()
        }
      }

      rem.foreach(_toposort)
      // need toSeq for 2.13, where Seq is immutable
      sorted.toSeq
    }

    @scala.annotation.tailrec
    def evaluateAll(
        evaluatees: Seq[Derived],
        creation: TopoTransaction,
        afterCommitObservers: ListBuffer[Observation]
    ): Seq[Derived] = {
      // first one where evaluation detects glitch
      val glitched = evaluatees.reverseIterator.find { r =>
        if (r.state.done) false
        else if (r.state.dirty) {
          Util.evaluate(r, creation, afterCommitObservers)
        } else false
      }
      glitched match {
        case None => evaluatees
        case Some(reactive) =>
          val evaluateNext = evaluatees.filterNot(_.state.done) ++ creation.initializer.drainCreated()
          evaluateNext.foreach(_.state.discovered = false)
          evaluateAll(Util.toposort(evaluateNext), creation, afterCommitObservers)
      }
    }

    def evaluate(
        reactive: Derived,
        creationTicket: TopoTransaction,
        afterCommitObservers: ListBuffer[Observation]
    ): Boolean = {
      var potentialGlitch = false
      val dt = new ReevTicket[State, reactive.Value](
        creationTicket,
        reactive.state.value,
        new AccessHandler[State] {
          override def dynamicAccess(input: ReSource): input.Value = {
            if (input.state.discovered && !input.state.done) {
              potentialGlitch = true
            }
            input.state.value
          }
          override def staticAccess(input: ReSource): input.Value = input.state.value
        }
      )
      val reev = reactive.reevaluate(dt)
      reev.inputs().foreach { newDeps =>
        val incoming = reactive.state.incoming
        reactive.state.incoming = newDeps
        val added   = newDeps diff incoming
        val removed = incoming diff newDeps
        added.foreach { input =>
          input.state.outgoing = input.state.outgoing + reactive
        }
        removed.foreach { input =>
          input.state.outgoing = input.state.outgoing - reactive
        }
      }

      if (potentialGlitch) true
      else {
        if (reev.activate) reactive.state.outgoing.foreach(_.state.dirty = true)
        reev.forValue(reactive.state.value = _)
        reev.forEffect(o => afterCommitObservers.append(o))
        reactive.state.done = true
        false
      }

    }
  }
}
