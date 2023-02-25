package rescala.extra.reactivestreams

import java.util.concurrent.Flow.{Publisher, Subscriber, Subscription}
import rescala.core.{Base, Derived, ReInfo, ReadAs, Scheduler, ScopeSearch}
import rescala.operator.Interface
import rescala.structure.Pulse

import java.util.Objects
import scala.util.{Failure, Success}

class ReactiveStreamsApi(val api: Interface) {
  import api._

  class RESubscriber[T](evt: Evt[T], fac: Scheduler[BundleState]) extends Subscriber[T] {

    var subscription: Subscription = _

    override def onError(thrw: Throwable): Unit =
      synchronized {
        Objects.requireNonNull(thrw)
        fac.forceNewTransaction(evt) { implicit turn => evt.admitPulse(Pulse.Exceptional(thrw)) }
      }
    override def onSubscribe(s: Subscription): Unit =
      synchronized {
        subscription = s
        subscription.request(1)
      }
    override def onComplete(): Unit = {}
    override def onNext(value: T): Unit =
      synchronized {
        Objects.requireNonNull(value)
        evt.fire(value)(fac, ScopeSearch.fromSchedulerImplicit(fac))
        subscription.request(1)
      }
  }

  class REPublisher[T](dependency: ReadAs.of[BundleState, Pulse[T]], fac: Scheduler[BundleState]) extends Publisher[T] {

    override def subscribe(s: Subscriber[_ >: T]): Unit = {
      val sub = REPublisher.subscription(dependency, s, fac)
      s.onSubscribe(sub)
    }

  }

  class SubscriptionReactive[T](
      bud: BundleState[Pulse[T]],
      dependency: ReadAs.of[BundleState, Pulse[T]],
      subscriber: Subscriber[_ >: T],
      name: ReInfo
  ) extends Base[BundleState, Pulse[T]](bud, name)
      with Derived
      with Subscription {

    override type State[V] = ReactiveStreamsApi.this.api.BundleState[V]

    var requested: Long = 0
    var cancelled       = false

    override protected[rescala] def reevaluate(rein: ReIn): Rout = {
      rein.dependStatic(dependency).toOptionTry match {
        case None => rein
        case Some(tryValue) =>
          synchronized {
            while (requested <= 0 && !cancelled) wait(100)
            if (cancelled) {
              rein.trackDependencies(Set.empty)
              rein
            } else {
              requested -= 1
              tryValue match {
                case Success(v) =>
                  subscriber.onNext(v)
                  rein
                case Failure(t) =>
                  subscriber.onError(t)
                  cancelled = true
                  rein.trackDependencies(Set.empty)
                  rein
              }
            }
          }
      }
    }

    override protected[rescala] def commit(base: Pulse[T]): Pulse[T] = base

    override def cancel(): Unit = {
      synchronized {
        cancelled = true
        notifyAll()
      }
    }

    override def request(n: Long): Unit =
      synchronized {
        requested += n
        notifyAll()
      }
  }

  object REPublisher {

    def apply[T](dependency: ReadAs.of[BundleState, Pulse[T]])(implicit fac: Scheduler[BundleState]): REPublisher[T] =
      new REPublisher[T](dependency, fac)

    def subscription[T](
        dependency: ReadAs.of[BundleState, Pulse[T]],
        subscriber: Subscriber[_ >: T],
        fac: Scheduler[BundleState]
    ): SubscriptionReactive[T] = {
      fac.forceNewTransaction() { ticket =>
        val name: ReInfo = ReInfo.create.derive(s"forSubscriber($subscriber)")
        ticket.tx.initializer.create[Pulse[T], SubscriptionReactive[T]](
          Set(dependency),
          Pulse.empty,
          needsReevaluation = false
        ) {
          state => new SubscriptionReactive[T](state, dependency, subscriber, name)
        }
      }
    }

  }
}
