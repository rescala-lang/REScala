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

  /**
   * LVars serve as the basis for reactive lenses. TODO: Link Documentation
   * @param state The state of the LVar
   * @param events TODO How to explain this?
   */
  class LVar[M] private[rescala](state : Signal[M], events : Evt[Event[M]]) {

    /**
     * Creates a new LVar which is connected to this LVar via the given Lens.
     * @param lens The lens which connects the LVars. Can use implicit conversion from BijectiveLens if the Lens does not need to change later
     */

    def applyLens[V](lens: BijectiveSigLens[M, V])(implicit ticket: CreationTicket[BundleState], sched: Scheduler[BundleState]): LVar[V] = {
      val newVar = new LVar[V](state.map { model => lens.toView(model) }.flatten, Evt())
      events.fire(newVar.getEvent().map { e => lens.toModel(e) })
      return newVar
    }

    /**
     * Register an event which should trigger a change of this LVar (and consequently the entire lens cluster)
     * @param e The event which should change the LVar
     */
    def fire(e: Event[M])(implicit sched: Scheduler[BundleState]) : Unit = events.fire(e)

    /**
     * Function to observe a change of the LVar. Simple wrapper for internal.observe
     */
    def observe(onValue: M => Unit, onError: Throwable => Unit = null, fireImmediately: Boolean = false)
               (implicit ticket: CreationTicket[BundleState]) = state.observe(onValue, onError, fireImmediately)

    //TODO Documentation
    def getEvent()(implicit ticket: CreationTicket[BundleState]) : Event[M] = events.list().flatten(firstFiringEvent)

    /**
     * Function to access the current value of the lens. Simple wrapper for internal.now
     */
    def now(implicit sched: Scheduler[BundleState]) : M = state.now

    /**
     * Function to access state of LVar in reactives. Simple wrapper for internal.value.
     */
    inline def value(implicit sched: Scheduler[BundleState]) : M = state.value

  }

  object LVar {

    /**
     * Creates a new LVar with the given initial value. This will be the root of a new Lens cluster
     * @param initval the inital value of the LVar
     */
    def apply[T](initval: T)(implicit ticket: CreationTicket[BundleState]): LVar[T] = {
      val events : Evt[Event[T]] = Evt()
      new LVar[T](events.list().flatten(firstFiringEvent).hold(initval), events)
    }

  }

  //TODO: Note that non-bijective lenses are not supported due to the chosen propagation. Only keep for demonstration
  trait Lens[M, V] {
    def toView(m: M): V
    def toModel(v: V, m: M): M
  }

  /**
   * The base trait for all bijective lenses
   * @tparam M The type of the model
   * @tparam V The type of the view
   */
  trait BijectiveLens[M, V] {
    /**
     * Transforms the model to the view
     */
    def toView(m: M): V

    /**
     * Transforms the view to the model
     */
    def toModel(v: V): M

    //TODO: Better way to pass this than via implicit?

    /**
     * Inverts the lens such that e.g. an AddLens functions like a SubLens. Note that this does not change the model-view relationship,
     * i.e. the asymmetry is not inverted.
     */
    def inverse(implicit lens: BijectiveLens[M, V] = this): BijectiveLens[V, M] = new BijectiveLens[V, M] {
      override def toView(m: V): M = lens.toModel(m)
      override def toModel(v: M): V = lens.toView(v)
    }

    /**
     * Concatenates this lens with another lens and returns the resulting lens.
     * @param other The other lens
     */
    def compose[W](other: BijectiveLens[V, W])(implicit lens: BijectiveLens[M, V] = this): BijectiveLens[M, W] = new BijectiveLens[M, W] {
      override def toView(m: M) : W = other.toView(lens.toView(m))
      override def toModel(w: W): M = lens.toModel(other.toModel(w))
    }
  }

  /**
   * TODO: The BijectiveSigLens requires a reactive read without evaluating dependencies. As this is currently not supported by REScala, it uses _now_ instead!
   * @param lensSig
   * @tparam M
   * @tparam V
   */
  class BijectiveSigLens[M, V](lensSig : Signal[BijectiveLens[M, V]])(implicit sched: Scheduler[BundleState]){
    def toView(m: M) : Signal[V] = lensSig.map { model => model.toView(m) }
    def toModel(v: V) : M = lensSig.now.toModel(v)
  }

  /**
   * Implicit conversion of a BijectiveLens to a BijectiveSigLens for uniform handling.
   * Could instead also be handled by method overloading (currently only LVar.applyLens(..)) for slightly more efficiency.
   */
  implicit def toSignalLens[M, V](lens: BijectiveLens[M, V])(implicit ticket: CreationTicket[BundleState],
                                                             sched: Scheduler[BundleState]): BijectiveSigLens[M, V] = BijectiveSigLens(Signal {lens})

  class AddLens[A](k: A)(implicit num: Numeric[A]) extends BijectiveLens[A, A] {
    def toView(m: A): A = num.plus(m, k)
    def toModel(v: A): A = num.minus(v, k)
  }

  class NeutralLens[A] extends BijectiveLens[A, A] {
    def toView(m: A): A = m
    def toModel(v: A): A = v
  }

  //IS NOT BIJECTIVE
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
