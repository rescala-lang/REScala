package rescala.reactivestreams


import org.reactivestreams.{Publisher, Subscriber, Subscription}
import rescala.core.{Base, Engine, Pulse, Pulsing, REName, Reactive, ReevaluationResult, Struct, Turn, ValuePersistency}

import scala.util.{Failure, Success}


object REPublisher {

  def apply[T, S <: Struct](dependency: Pulsing[Pulse[T], S])(implicit fac: Engine[S]): REPublisher[T, S] = new REPublisher[T, S](dependency, fac)


  class REPublisher[T, S <: Struct](dependency: Pulsing[Pulse[T], S], fac: Engine[S]) extends Publisher[T] {

    override def subscribe(s: Subscriber[_ >: T]): Unit = {
      val sub = REPublisher.subscription(dependency, s, fac)
      s.onSubscribe(sub)
    }

  }

  class SubscriptionReactive[T, S <: Struct](bud: S#State[Pulse[T], S], dependency: Pulsing[Pulse[T], S], subscriber: Subscriber[_ >: T], fac: Engine[S], name: REName) extends Base[T, S](bud, name) with Reactive[S] with Subscription {

    var requested: Long = 0
    var cancelled = false

    override protected[rescala] def reevaluate(ticket: Turn[S], before: Pulse[T], indeps: Set[Reactive[S]]): ReevaluationResult[Value, S] = {
      ticket.makeStaticReevaluationTicket().staticDepend(dependency).toOptionTry match {
        case None => ReevaluationResult.Static(Pulse.NoChange)
        case Some(tryValue) =>
          synchronized {
            while (requested <= 0 && !cancelled) wait(100)
            if (cancelled) ReevaluationResult.Dynamic[T, S](Pulse.NoChange, indepsAfter = Set.empty, indepsAdded = Set.empty, indepsRemoved = Set(dependency))
            else {
              requested -= 1
              tryValue match {
                case Success(v) =>
                  subscriber.onNext(v)
                  ReevaluationResult.Static(Pulse.NoChange)
                case Failure(t) =>
                  subscriber.onError(t)
                  cancelled = true
                  ReevaluationResult.Dynamic[T, S](Pulse.NoChange, indepsAfter = Set.empty, indepsAdded = Set.empty, indepsRemoved = Set(dependency))
              }
            }
          }
      }
    }

    override def cancel(): Unit = {
      synchronized {
        cancelled = true
        notifyAll()
      }
    }

    override def request(n: Long): Unit = synchronized {
      requested += n
      notifyAll()
    }
  }

  def subscription[T, S <: Struct](dependency: Pulsing[Pulse[T], S], subscriber: Subscriber[_ >: T], fac: Engine[S]): SubscriptionReactive[T, S] = {
    val incoming = Set[Reactive[S]](dependency)
    fac.transaction(dependency) { ticket =>
      ticket.creation.create[Pulse[T], SubscriptionReactive[T, S]](incoming, ValuePersistency.DerivedSignal) { state =>
        new SubscriptionReactive[T, S](state, dependency, subscriber, fac, s"forSubscriber(${subscriber})")
      }
    }
  }

}
