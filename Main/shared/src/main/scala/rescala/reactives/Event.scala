package rescala.reactives

import rescala.core.{CreationTicket, DynamicTicket, Engine, Pulse, Pulsing, ReSerializable, Reactive, Struct}
import rescala.core.Pulse.{Exceptional, NoChange, Value}
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, UnhandledFailureException}

import scala.annotation.compileTimeOnly
import scala.collection.immutable.{LinearSeq, Queue}

/**
  * Base signal interface for all signal implementations.
  * Please note that any event implementation should have the EV type parameter set to itself and be paired with
  * exactly one signal implementation it is compatible with by setting the SL type parameter.
  * This relationship needs to be symmetrical.
  *
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
trait Event[+T, S <: Struct] extends Pulsing[Pulse[T], S] with Observable[T, S] {

  @compileTimeOnly("Event.apply can only be used inside of Signal expressions")
  def apply(): Option[T] = throw new IllegalAccessException(s"$this.apply called outside of macro")

  def disconnect()(implicit engine: Engine[S]): Unit


  /** collect results from a partial function */
  final def collect[U](pf: PartialFunction[T, U])(implicit ticket: CreationTicket[S]): Event[U, S] = Events.static(s"(collect $this)", this) { st => st.staticDepend(this).collect(pf) }

  /** add an observer */
  final def +=(react: T => Unit)(implicit ticket: CreationTicket[S]): Observe[S] = observe(react)(ticket)


  final def recover[R >: T](onFailure: PartialFunction[Throwable,Option[R]])(implicit ticket: CreationTicket[S]): Event[R, S] = Events.static(s"(recover $this)", this) { st =>
    st.staticDepend(this) match {
      case Exceptional(t) => onFailure.applyOrElse[Throwable, Option[R]](t, throw _).fold[Pulse[R]](Pulse.NoChange)(Pulse.Value(_))
      case other => other
    }
  }


  final def abortOnError()(implicit ticket: CreationTicket[S]): Event[T, S] = recover{case t => throw new UnhandledFailureException(this, t)}


  /** Events disjunction. */
  final def ||[U >: T](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[U, S] = {
    Events.static(s"(or $this $other)", this, other) { st =>
      val tp = st.staticDepend(this)
      if (tp.isChange) tp else st.staticDepend(other)
    }
  }

  /** EV filtered with a predicate */
  final def filter(pred: T => Boolean)(implicit ticket: CreationTicket[S]): Event[T, S] = Events.static(s"(filter $this)", this) { st => st.staticDepend(this).filter(pred) }
  /** EV filtered with a predicate */
  final def &&(pred: T => Boolean)(implicit ticket: CreationTicket[S]): Event[T, S] = filter(pred)

  /** EV is triggered except if the other one is triggered */
  final def \[U](except: Event[U, S])(implicit ticket: CreationTicket[S]): Event[T, S] = {
    Events.static(s"(except $this  $except)", this, except) { st =>
      st.staticDepend(except) match {
        case NoChange => st.staticDepend(this)
        case Value(_) => Pulse.NoChange
        case ex@Exceptional(_) => ex
      }
    }
  }

  /** Events conjunction */
  final def and[U, R](other: Event[U, S])(merger: (T, U) => R)(implicit ticket: CreationTicket[S]): Event[R, S] = {
    Events.static(s"(and $this $other)", this, other) { st =>
      for {
        left <- st.staticDepend(this)
        right <- st.staticDepend(other)
      } yield {merger(left, right)}
    }
  }

  /** Event conjunction with a merge method creating a tuple of both event parameters */
  final def zip[U](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[(T, U), S] = and(other)(Tuple2.apply)

  /** Event disjunction with a merge method creating a tuple of both optional event parameters wrapped */
  final def zipOuter[U](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[(Option[T], Option[U]), S] = {
    Events.static(s"(zipOuter $this $other)", this, other) { st =>
      val left = st.staticDepend(this)
      val right = st.staticDepend(other)
      if(right.isChange || left.isChange) Value(left.toOption -> right.toOption) else NoChange
    }
  }

  /** Transform the event parameter */
  final def map[U](mapping: T => U)(implicit ticket: CreationTicket[S]): Event[U, S] = Events.static(s"(map $this)", this) {  st => st.staticDepend(this).map(mapping) }

  final def dMap[U](mapping: DynamicTicket[S] => T => U)(implicit ticket: CreationTicket[S]): Event[U, S] = Events.dynamic(this) {
    dt => dt.depend(this).map(v => mapping(dt)(v))
  }

  /** Drop the event parameter; equivalent to map((_: Any) => ()) */
  final def dropParam(implicit ticket: CreationTicket[S]): Event[Unit, S] = map(_ => ())


  /** folds events with a given fold function to create a Signal */
  final def fold[A: ReSerializable](init: A)(folder: (A, T) => A)(implicit ticket: CreationTicket[S]): Signal[A, S] = {
    def f(a: => A, t: => T) = folder(a, t)
    lazyFold(init)(f)
  }

  /** folds events with a given fold function to create a Signal allowing recovery of exceptional states by ignoring the stable value */
  final def lazyFold[A: ReSerializable](init: => A)(folder: (=> A, => T) => A)(implicit ticket: CreationTicket[S]): Signal[A, S] = ticket { initialTurn =>
    Signals.staticFold[A, S](Set[Reactive[S]](this), _ => init) { (st, currentValue) =>
      // TODO this should be equivalent, but doesn't work for unmanaged engine; need to investigate.
      // folder(currentValue, st.turn.after(this).get)
      st.staticDepend(this).toOption.fold(currentValue)(value => folder(currentValue, value))
    }(initialTurn)(ticket.rename)
  }

  /** reduces events with a given reduce function to create a Signal */
  final def reduce[A: ReSerializable](reducer: (=> A, => T) => A)(implicit ticket: CreationTicket[S]): Signal[A, S] =
    lazyFold(throw EmptySignalControlThrowable)(reducer)

  /** Applies a function on the current value of the signal every time the event occurs,
    * starting with the init value before the first event occurrence */
  final def iterate[A: ReSerializable](init: A)(f: A => A)(implicit ticket: CreationTicket[S]): Signal[A, S] =
    fold(init)((acc, _) => f(acc))

  /**
    * Counts the occurrences of the event. Starts from 0, when the event has never been
    * fired yet. The argument of the event is simply discarded.
    */
  final def count()(implicit ticket: CreationTicket[S], ev: ReSerializable[Int]): Signal[Int, S] =
    fold(0)((acc, _) => acc + 1)

  /**
    * Calls f on each occurrence of event e, setting the SL to the generated value.
    * The initial signal is obtained by f(init)
    */
  final def set[B >: T : ReSerializable, A](init: B)(f: (B => A))(implicit ticket: CreationTicket[S]): Signal[A, S] =
    latest(init).map(f)

  /** returns a signal holding the latest value of the event. */
  final def latest[T1 >: T : ReSerializable](init: T1)(implicit ticket: CreationTicket[S]): Signal[T1, S] =
    fold(init)((_, v) => v)
  final def latest[T1 >: T : ReSerializable]()(implicit ticket: CreationTicket[S]): Signal[T1, S] =
    reduce[T1]((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured */
  final def latestOption[T1 >: T]()(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[T1]]): Signal[Option[T1], S] =
    fold(None: Option[T1]) { (_, v) => Some(v) }

  /** calls factory on each occurrence of event e, resetting the SL to a newly generated one */
  final def reset[T1 >: T : ReSerializable, A, R](init: T1)(factory: T1 => Signal[A, S])(implicit ticket: CreationTicket[S], ev: Flatten[Signal[A, S], S, R]): R =
    set(init)(factory).flatten(ev, ticket)

  /**
    * Returns a signal which holds the last n events in a list. At the beginning the
    * list increases in size up to when n values are available
    */
  final def last[T1 >: T](n: Int)(implicit ticket: CreationTicket[S], ev: ReSerializable[Queue[T1]]): Signal[LinearSeq[T1], S] = {
    fold(Queue[T1]()) { (queue: Queue[T1], v: T) =>
      if (queue.length >= n) queue.tail.enqueue(v) else queue.enqueue(v)
    }
  }

  /** collects events resulting in a variable holding a list of all values. */
  final def list[T1 >: T]()(implicit ticket: CreationTicket[S], ev: ReSerializable[List[T1]]): Signal[List[T1], S] =
    fold(List[T1]())((acc, v) => v :: acc)

  /** Switch back and forth between two signals on occurrence of event e */
  final def toggle[A](a: Signal[A, S], b: Signal[A, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Boolean]): Signal[A, S] = ticket { ict =>
    val switched: Signal[Boolean, S] = iterate(false) {!_}(ev, ict)
    Signals.dynamic(switched, a, b) { s => if (s.dynamicDepend(switched).get) s.dynamicDepend(b).get else s.dynamicDepend(a).get }(ict)
  }

  /** Return a Signal that is updated only when e fires, and has the value of the signal s */
  final def snapshot[A: ReSerializable](s: Signal[A, S])(implicit ticket: CreationTicket[S]): Signal[A, S] = ticket {
    Signals.staticFold[A, S](
      Set[Reactive[S]](this, s),
      st => st.staticDepend(s).get) { (st, current) =>
      st.staticDepend(this).toOption.fold(current)(_ => st.staticDepend(s).get)
    }(_)(ticket.rename)
  }


  /** Switch to a new Signal once, on the occurrence of event e. */
  final def switchOnce[A, T1 >: T](original: Signal[A, S], newSignal: Signal[A, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[T1]]): Signal[A, S] = ticket { turn =>
    val latest = latestOption[T1]()(turn, ev)
    Signals.dynamic(latest, original, newSignal) { t =>
      t.dynamicDepend(latest).get match {
        case None => t.dynamicDepend(original).get
        case Some(_) => t.dynamicDepend(newSignal).get
      }
    }(turn)
  }

  /**
    * Initially the result signal has the value of the original signal.
    * Every time the event fires, the result signal changes to the value of the event,
    * the original signal is no longer used.
    */
  final def switchTo[T1 >: T](original: Signal[T1, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[T1]]): Signal[T1, S] = ticket { turn =>
    val latest = latestOption[T1]()(turn, ev)
    Signals.dynamic(latest, original) { s =>
      s.dynamicDepend(latest).get match {
        case None => s.dynamicDepend(original).get
        case Some(x) => x
      }
    }(turn)
  }

  /** Like latest, but delays the value of the resulting signal by n occurrences */
  final def delay[T1 >: T](init: => T1, n: Int)(implicit ticket: CreationTicket[S], ev : ReSerializable[Queue[T1]]): Signal[T1, S] = ticket { turn =>
    lazy val initL = init
    val history: Signal[LinearSeq[T1], S] = last[T1](n + 1)(turn, ev)
    history.map { h => if (h.size <= n) initL else h.head }(turn)
  }

  /** returns the values produced by the last event produced by mapping this value */
  //final def flatMap[B](f: T => Event[B, S])(implicit ticket: CreationTicket[S]): Event[B, S] = ticket { implicit ict => map(f).latest(Evt[B, S]).flatten }

  /** promotes the latest inner event to an outer event */
  //final def flatten[B](implicit ticket: CreationTicket[S], ev: T <:< Event[B, S]): Event[B, S] = flatMap(ev.apply)

}
