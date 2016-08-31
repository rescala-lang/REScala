package rescala.reactivestreams


import org.reactivestreams.{Publisher, Subscriber, Subscription}
import rescala.engines.Engine
import rescala.graph._
import rescala.propagation.Turn

import scala.util.{Failure, Success}


object REPublisher {

  def apply[T, S <: Struct](dependency: Pulsing[T, S])(implicit fac: Engine[S, Turn[S]]): REPublisher[T, S] = new REPublisher[T, S](dependency, fac)


  class REPublisher[T, S <: Struct](dependency: Pulsing[T, S], fac: Engine[S, Turn[S]]) extends Publisher[T] {

    override def subscribe(s: Subscriber[_ >: T]): Unit = {
      val sub = REPublisher.subscription(dependency, s, fac)
      s.onSubscribe(sub)
    }

  }

  class SubscriptionReactive[T, S <: Struct](bud: S#SporeP[T, Reactive[S]], dependency: Pulsing[T, S], subscriber: Subscriber[_ >: T], fac: Engine[S, Turn[S]]) extends Base[T, S](bud) with Reactive[S] with Subscription {

    var requested: Long = 0
    var cancelled = false

    override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] = {
      if (turn.incoming(bud).isEmpty) ReevaluationResult.Dynamic(changed = false, DepDiff(Set.empty, Set(dependency)))
      else {
        dependency.pulse(turn).toOptionTry(asSignal = false) match {
          case None => ReevaluationResult.Static(changed = false)
          case Some(tryValue) =>
            synchronized {
              while (requested <= 0 && !cancelled) wait(100)
              if (cancelled) ReevaluationResult.Dynamic(changed = false, DepDiff[S](Set.empty, Set(dependency)))
              else {
                requested -= 1
                tryValue match {
                  case Success(v) =>
                    subscriber.onNext(v)
                    ReevaluationResult.Static(changed = false)
                  case Failure(t) =>
                    subscriber.onError(t)
                    cancelled = true
                    ReevaluationResult.Dynamic(changed = false, DepDiff[S](Set.empty, Set(dependency)))
                }
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

  def subscription[T, S <: Struct](dependency: Pulsing[T, S], subscriber: Subscriber[_ >: T], fac: Engine[S, Turn[S]]): SubscriptionReactive[T, S] = {
    val incoming = Set[Reactive[S]](dependency)
    fac.plan(dependency)(initTurn => initTurn.create(incoming) {
      new SubscriptionReactive[T, S](initTurn.bud[T, Reactive[S]](initialIncoming = incoming, transient = false), dependency, subscriber, fac)
    })
  }

}
