package rescala.operator

import rescala.core.{AdmissionTicket, Base, CreationTicket, InitialChange, Observation, ReInfo, ReSource, Scheduler, ScopeSearch}
import rescala.structure.Pulse

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

  class LVar[M] private[rescala](initialState: BundleState[Pulse[M]], name: ReInfo, model : Var[M], lens: Lens[M, M])
    extends Var(initialState, name) {

    override def set(value : M)(implicit sched: Scheduler[State], scopeSearch: ScopeSearch[State]): Unit = {
      model.set(lens.toModel(value, this.now))
    }

    def applyLens(lens: Lens[M, M])(implicit ticket: CreationTicket[BundleState], sched: Scheduler[State]): LVar[M] = {
      ticket.createSource[Pulse[M], LVar[M]](Pulse.Value(lens.toView(this.now)))(s => new LVar[M](s, ticket.info, this, lens))
//      val (inputs, fun, isStatic) =
//        rescala.macros.getDependencies[T, ReSource.of[BundleState], rescala.core.StaticTicket[BundleState], true](expr)
//      ticket.create[Pulse[M], SignalImpl[BundleState, M] with Signal[M]](
//        inputs.toSet,
//        Pulse.empty,
//        needsReevaluation = true
//      ) {
//        state => new SignalImpl(state, (t, _) => expr(t), ct.info, None) with Signal[T]
//      }
    }

  }

  object LVar {
    def apply[T](initval: T)(implicit ticket: CreationTicket[BundleState]): LVar[T] = fromChange(Pulse.Value(initval))

    def empty[T](implicit ticket: CreationTicket[BundleState]): LVar[T] = fromChange(Pulse.empty)

    private[this] def fromChange[T](change: Pulse[T])(implicit ticket: CreationTicket[BundleState]): LVar[T] = {
      ticket.createSource[Pulse[T], LVar[T]](change)(s => new LVar[T](s, ticket.info, new Var[T](s, ticket.info), new NeutralLens[T]()))
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

  class AddLens[A](k: A)(implicit num: Numeric[A]) extends BijectiveLens[A, A] {
    def toView(m: A): A = num.plus(m, k)
    def toModel(v: A): A = num.minus(v, k)
  }

  class NeutralLens[A] extends BijectiveLens[A, A] {
    def toView(m: A): A = m

    def toModel(v: A): A = v
  }


}
