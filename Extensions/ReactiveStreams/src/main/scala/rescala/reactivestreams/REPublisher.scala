package rescala.reactivestreams


import org.reactivestreams.{Publisher, Subscriber, Subscription}
import rescala.engine.Engine
import rescala.graph._
import rescala.propagation.Turn

import scala.util.{Failure, Success}


object REPublisher {

  def apply[T, S <: Struct](dependency: Pulsing[Pulse[T], S])(implicit fac: Engine[S, Turn[S]]): REPublisher[T, S] = new REPublisher[T, S](dependency, fac)


  class REPublisher[T, S <: Struct](dependency: Pulsing[Pulse[T], S], fac: Engine[S, Turn[S]]) extends Publisher[T] {

    override def subscribe(s: Subscriber[_ >: T]): Unit = {
      val sub = REPublisher.subscription(dependency, s, fac)
      s.onSubscribe(sub)
    }

  }

  class SubscriptionReactive[T, S <: Struct](bud: S#Type[Pulse[T], S], dependency: Pulsing[Pulse[T], S], subscriber: Subscriber[_ >: T], fac: Engine[S, Turn[S]]) extends Base[T, S](bud) with Reactive[S] with Subscription {

    var requested: Long = 0
    var cancelled = false

    override protected[rescala] def reevaluate(ticket: S#Ticket[S]): ReevaluationResult[Value, S] = {
      dependency.pulse(ticket).toOptionTry match {
        case None => ReevaluationResult.Static(Pulse.NoChange)
        case Some(tryValue) =>
          synchronized {
            while (requested <= 0 && !cancelled) wait(100)
            if (cancelled) ReevaluationResult.Dynamic[T, S](Pulse.NoChange, Set.empty)
            else {
              requested -= 1
              tryValue match {
                case Success(v) =>
                  subscriber.onNext(v)
                  ReevaluationResult.Static(Pulse.NoChange)
                case Failure(t) =>
                  subscriber.onError(t)
                  cancelled = true
                  ReevaluationResult.Dynamic[T, S](Pulse.NoChange, Set.empty)
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

  def subscription[T, S <: Struct](dependency: Pulsing[Pulse[T], S], subscriber: Subscriber[_ >: T], fac: Engine[S, Turn[S]]): SubscriptionReactive[T, S] = {
    val incoming = Set[Reactive[S]](dependency)
    fac.plan(dependency)(initTurn => initTurn.create(incoming) {
      new SubscriptionReactive[T, S](initTurn.makeStructState[Pulse[T]](Pulse.NoChange, initialIncoming = incoming, transient = false), dependency, subscriber, fac)
    })
  }

}
