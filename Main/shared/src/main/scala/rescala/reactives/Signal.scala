package rescala.reactives

import rescala.engine.{Engine, TicketOrEngine, Turn, TurnSource}
import rescala.graph._
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, UnhandledFailureException}
import rescala.reactives.Signals.Diff

import scala.annotation.compileTimeOnly
import scala.util.control.NonFatal

object Signal {
  private def now0[A](pulse: Pulse[A]): A = {
    try { pulse.get }
    catch {
      case EmptySignalControlThrowable => throw new NoSuchElementException(s"Signal $this is empty")
      case other: Throwable => throw new IllegalStateException("Signal has an error value", other)
    }
  }
}
/**
  * Base signal interface for all signal implementations.
  * Please note that any signal implementation should have the SL type parameter set to itself and be paired with
  * exactly one event implementation it is compatible with by setting the EV type parameter.
  * This relationship needs to be symmetrical.
  *
  * @tparam A Type stored by the signal
  * @tparam S Struct type used for the propagation of the signal
  */
trait Signal[+A, S <: Struct] extends Pulsing[Pulse[A], S] with Observable[A, S] {

  // only used inside macro and will be replaced there
  @compileTimeOnly("Signal.apply can only be used inside of Signal expressions")
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  // ========== Well-Defined, Non-Implicit variants of now/after/before ==========
  final def now(engine: Engine[S, _ <: Turn[S]]): A = Signal.now0(engine.singleNow(this))
  final def now(ticket: OutsidePropagationTicket[S]): A = Signal.now0(ticket.now(this))
  final def before(ticket: AlwaysTicket[S]): A = {
    try { ticket.before(this).get }
    catch { case EmptySignalControlThrowable => throw new NoSuchElementException(s"Signal $this is empty") }
  }
  final def after(ticket: PropagationAndLaterTicket[S]): A = {
    try { ticket.after(this).get }
    catch { case EmptySignalControlThrowable => throw new NoSuchElementException(s"Signal $this is empty") }
  }

  // ========== Less-Well-Defined, Implicit variants of now/after/before ==========
  // thoughts regarding implicit parameter variants of .now, .before and .after:
  // before: can be used at any time during turns. Thus its parameter should accept implicit turns only.
  //         However, we want to be able to invoke .before inside, e.g., the closure of a .fold, where the turn
  //         is not present in the scope. Thus before also accepts engines, but dynamically checks, that this engine
  //         has a current turn set. This could probably be ensured statically by making sure that every reactive
  //         definition site somehow provides the reevaluating ticket as an implicit in the scope, but I'm not sure
  //         if this is possible without significant syntactical inconvenience.
  //  after: falls under the same considerations as before, with the added exception that it should only accept
  //         turns that completed their admission phase and started their propagation phase. This is currently not
  //         checked at all, but could also be ensured statically the same as above.
  //    now: can be used inside turns only during admission and wrapup, or outside of turns at all times. Since during
  //         admission and wrapup, a corresponding OutsidePropagationTicket is in scope, it accepts these tickets as
  //         high priority implicit parameters. In case such a ticket is not available, it accepts engines, and then
  //         dynamically checks that the engine does NOT have a current turn (in the current thread context). I think
  //         this cannot be ensured statically, as users can always hide implicitly available current turns.
  final def now(implicit ticketOrEngine: TicketOrEngine[S]): A = {
    ticketOrEngine.e match {
      case Left(ticket) => now(ticket)
      case Right(engine) => if(engine.currentTurn().isDefined) {
        throw new IllegalAccessException("reactive.now has undefined behavior and thus cannot be used inside turns, but outside admission or wrapUp phase." +
          "If this error wrongly shows up during admission or wrapup of a turn, make sure that the implicit admission or wrapup ticket is in scope?")
      } else {
        now(engine)
      }
    }
  }

  final def before(implicit ticket: TurnSource[S]): A = {
    val turn = ticket.requireCurrentTurn.getOrElse {
      throw new IllegalAccessException("must not invoke reactive.before outside of turns; we should find a way to ensure this syntactically...")
    }
    before(turn)
  }

  final def after(implicit ticket: TurnSource[S]): A = {
    val currentTurn = ticket.requireCurrentTurn.getOrElse{
      throw new IllegalAccessException("must not invoke reactive.after outside of turns; we should find a way to ensure this syntactically...")
    }
    // TODO it should be checked that the turn is actually in Propagation or some later phase..
    after(new PropagationAndLaterTicket[S]{ val turn = currentTurn })
  }

  final def recover[R >: A](onFailure: PartialFunction[Throwable,R])(implicit ticket: TurnSource[S]): Signal[R, S] = Signals.static(this) { st =>
    try st.staticDepend(this).get catch {
      case NonFatal(e) => onFailure.applyOrElse[Throwable, R](e, throw _)
    }
  }

  // ================== Derivations ==================

  //final def recover[R >: A](onFailure: Throwable => R)(implicit ticket: TurnSource[S]): Signal[R, S] = recover(PartialFunction(onFailure))

  final def abortOnError()(implicit ticket: TurnSource[S]): Signal[A, S] = recover{case t => throw new UnhandledFailureException(this, t)}

  final def withDefault[R >: A](value: R)(implicit ticket: TurnSource[S]): Signal[R, S] = Signals.static(this) { (st) =>
    try st.staticDepend(this).get catch {
      case EmptySignalControlThrowable => value
    }
  }

  def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit

  /** Return a Signal with f applied to the value */
  final def map[B](f: A => B)(implicit ticket: TurnSource[S]): Signal[B, S] = Signals.lift(this)(f)

  /** flatten the inner reactive */
  final def flatten[R](implicit ev: Flatten[A, S, R], ticket: TurnSource[S]): R = ev.apply(this)(ticket)

  /** Delays this signal by n occurrences */
  final def delay(n: Int)(implicit ticket: TurnSource[S]): Signal[A, S] = ticket { implicit turn => changed.delay(before(turn), n) }

  /** Create an event that fires every time the signal changes. It fires the tuple (oldVal, newVal) for the signal.
    * Be aware that no change will be triggered when the signal changes to or from empty */
  final def change(implicit ticket: TurnSource[S]): Event[Diff[A], S] = Events.change(this)

  /**
    * Create an event that fires every time the signal changes. The value associated
    * to the event is the new value of the signal
    */
  final def changed(implicit ticket: TurnSource[S]): Event[A, S] = Events.static(s"(changed $this)", this) { st =>
    st.staticDepend(this) match {
      case Pulse.empty => Pulse.NoChange
      case other => other
    }
  }

  /** Convenience function filtering to events which change this reactive to value */
  final def changedTo[V](value: V)(implicit ticket: TurnSource[S]): Event[Unit, S] = (changed filter {_ == value}).dropParam
}

