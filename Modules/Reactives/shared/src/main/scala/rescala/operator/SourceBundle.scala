package rescala.operator

import rescala.core.{AdmissionTicket, Base, CreationTicket, InitialChange, Observation, ReInfo, ReSource, Scheduler, ScopeSearch}
import rescala.structure.Pulse

//import scala.language.implicitConversions
import scala.util.Random

trait SourceBundle {
  self: Operators =>

  trait Source[T] extends rescala.core.ReSource {
    final def admit(value: T)(implicit ticket: AdmissionTicket[State]): Unit = admitPulse(Pulse.Value(value))
    def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket[State]): Unit
  }

  /** Source events with imperative occurrences
    *
    * @param initialState of by the event
    * @tparam T Type returned when the event fires
    * @tparam S Struct type used for the propagation of the event
    */
  class Evt[T] private[rescala] (initialState: BundleState[Pulse[T]], name: ReInfo)
      extends Base[BundleState, Pulse[T]](initialState, name)
      with Source[T]
      with Event[T] {
    override type Value = Pulse[T]

    override protected[rescala] def commit(base: Value): Value = Pulse.NoChange

    override def internalAccess(v: Pulse[T]): Pulse[T] = v

    /** Trigger the event */
    @deprecated("use .fire instead of apply", "0.21.0")
    def apply(value: T)(implicit fac: Scheduler[State], scopeSearch: ScopeSearch[State]): Unit        = fire(value)
    def fire()(implicit fac: Scheduler[State], scopeSearch: ScopeSearch[State], ev: Unit =:= T): Unit = fire(ev(()))
    def fire(value: T)(implicit sched: Scheduler[State], scopeSearch: ScopeSearch[State]): Unit =
      scopeSearch.maybeTransaction match {
        case None => sched.forceNewTransaction(this) { admit(value)(_) }
        case Some(tx) => tx.observe(new Observation {
            override def execute(): Unit = sched.forceNewTransaction(Evt.this) { admit(value)(_) }
          })
      }
    override def disconnect(): Unit = ()
    def admitPulse(pulse: Pulse[T])(implicit ticket: AdmissionTicket[State]): Unit = {
      ticket.recordChange(new InitialChange[State] {
        override val source: Evt.this.type = Evt.this
        override def writeValue(base: Pulse[T], writeCallback: Pulse[T] => Unit): Boolean = {
          writeCallback(pulse); true
        }
      })
    }
  }

  /** @group create */
  final def Evt[A]()(implicit ticket: CreationTicket[BundleState]): Evt[A] = {
    ticket.createSource[Pulse[A], Evt[A]](Pulse.NoChange)(init => { new Evt[A](init, ticket.info) }: Evt[A])
  }

  /** Source signals with imperatively updates.
    *
    * @tparam A Type stored by the signal
    */
  class Var[A] private[rescala] (initialState: BundleState[Pulse[A]], name: ReInfo)
      extends Base[BundleState, Pulse[A]](initialState, name)
      with Source[A] with Signal[A] {
    override type Value = Pulse[A]

    override def disconnect(): Unit = ()

    def set(value: A)(implicit sched: Scheduler[State], scopeSearch: ScopeSearch[State]): Unit =
      scopeSearch.maybeTransaction match {
        case None => sched.forceNewTransaction(this) { admit(value)(_) }
        case Some(tx) => tx.observe(new Observation {
            override def execute(): Unit = sched.forceNewTransaction(Var.this) { admit(value)(_) }
          })
      }

    def transform(f: A => A)(implicit sched: Scheduler[State], scopeSearch: ScopeSearch[State]): Unit = {
      def newTx() = sched.forceNewTransaction(this) { t =>
        admit(f(t.tx.now(this)))(t)
      }
      scopeSearch.maybeTransaction match {
        case None     => newTx()
        case Some(tx) => tx.observe(new Observation { override def execute(): Unit = newTx() })
      }
    }

    def setEmpty()(implicit fac: Scheduler[State]): Unit =
      fac.forceNewTransaction(this)(t => admitPulse(Pulse.empty)(t))

    def admitPulse(pulse: Pulse[A])(implicit ticket: AdmissionTicket[State]): Unit = {
      ticket.recordChange(new InitialChange[State] {
        override val source: Var.this.type = Var.this
        override def writeValue(base: Pulse[A], writeCallback: Pulse[A] => Unit): Boolean =
          if (base != pulse) { writeCallback(pulse); true }
          else false
      })
    }
  }

  /** Creates new [[Var]]s
    * @group create
    */
  object Var {
    def apply[T](initval: T)(implicit ticket: CreationTicket[BundleState]): Var[T] = fromChange(Pulse.Value(initval))
    def empty[T](implicit ticket: CreationTicket[BundleState]): Var[T]             = fromChange(Pulse.empty)
    private[this] def fromChange[T](change: Pulse[T])(implicit ticket: CreationTicket[BundleState]): Var[T] = {
      ticket.createSource[Pulse[T], Var[T]](change)(s => new Var[T](s, ticket.info))
    }
  }

/*
On Determinism:
Due to the chosen propagation strategy all lens projections must be deterministic, as non-deterministic behaviour may override user Inputs,
e.g. see the CharLens. There are two approaches two fix this issue. The first is an update the propagation mechanism which would need to be
developed from scratch as the current implementation cannot allow for the exclusion of the original path. The second way is to include the View
state in the lens, i.e. change the function signature to toView(m: M, v : V). Then, in the example of the CharLens, the check v.length == m
could prevent an update, though this might not always be the intended behaviour (i.e. a button to regenerate password should give a new password,
even if the length did not change).

On State in Lenses:
To me, the inclusion of the model state in the default lens seems to be unnecessary. In my opinion, the state should be included in the LVar instead,
where a fold on getEvent() should allow for the same functionality AFIK. This separates functionality more cleanly on the rescala transaction side
and reduces the complexity of the Lenses, by reducing them to their "pure" form of a simple bijective mapping.
 */

  class LVar[M] private[rescala](val internal : Signal[M], events : Evt[Event[M]]) {
    type T = M

    def applyLens[V](lens : Lens[M, V])(implicit ticket: CreationTicket[BundleState], sched: Scheduler[BundleState]) : LVar[V] = {
      val newVar = new LVar[V](Signal{lens.toView(internal.value)}, Evt())
      events.fire(newVar.getEvent().map{ e => lens.toModel(e, internal.now) })
      return newVar
    }

    //TODO make not bijective
    def applyLens[V](lens: BijectiveSigLens[M, V])(implicit ticket: CreationTicket[BundleState], sched: Scheduler[BundleState]): LVar[V] = {
      val newVar = new LVar[V](Signal{lens.toView(internal.value)}.flatten, Evt())
      events.fire(newVar.getEvent().map { e => lens.toModel(e, internal.now) })
      return newVar
    }

    def fire(e: Event[M])(implicit sched: Scheduler[BundleState]) : Unit = events.fire(e)

    def observe(onValue: M => Unit, onError: Throwable => Unit = null, fireImmediately: Boolean = false)
               (implicit ticket: CreationTicket[BundleState]) = internal.observe(onValue, onError, fireImmediately)

    def getEvent()(implicit ticket: CreationTicket[BundleState]) : Event[M] = events.list().flatten(firstFiringEvent)//.map(_.flatten.head)

    def now(implicit sched: Scheduler[internal.State]) : M = internal.now

    inline def value(implicit sched: Scheduler[BundleState]) : M = internal.value

  }

  object LVar {

    def apply[T](initval: T)(implicit ticket: CreationTicket[BundleState]): LVar[T] = {
      val events : Evt[Event[T]] = Evt()
      new LVar[T](events.list().flatten(firstFiringEvent).hold(initval), events)
    }

  }

  trait Lens[M, V] {
    def toView(m: M): V
    def toModel(v: V, m: M): M
  }

  trait BijectiveLens[M, V] extends Lens[M, V] {
    def toView(m: M): V
    def toModel(v: V): M
    def toModel(v: V, m: M): M = toModel(v)
  }

  class BijectiveSigLens[M, V](lensSig : Signal[BijectiveLens[M, V]])(implicit sched: Scheduler[lensSig.State]){
    def toView(m: M) : Signal[V] = Signal{ lensSig.value.toView(m)}
    def toModel(v: V) : M = lensSig.now.toModel(v)
    def toModel(v: V, m: M): M = toModel(v)
  }

  //given Conversion[BijectiveLens[M, V], BijectiveSigLens[M, V]] = new BijectiveSigLens(Signal{ _ })

  class AddLens[A](k: A)(implicit num: Numeric[A]) extends BijectiveLens[A, A] {
    def toView(m: A): A = num.plus(m, k)
    def toModel(v: A): A = num.minus(v, k)
  }

  class NonDeterministicCharCountLens extends BijectiveLens[Int, String] {
    // Lens that converts an int to a sequence of chars, eg. 5 => "aaaaa"
    def toView(m: Int): String = Random.alphanumeric.take(m).mkString
    def toModel(v: String): Int = v.length
  }

  class UpperCharLens extends BijectiveLens[String, String]{
    //shift up a char's letters to all caps
    //würde aber eh noch so "if-checks" benötigen, dass man vorher überprüft obs schon upperCase ist or not
    def toView(m: String): String = m.toUpperCase()
    def toModel(v: String): String = v.toLowerCase()
  }
}
