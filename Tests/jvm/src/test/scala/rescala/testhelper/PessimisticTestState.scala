package rescala.testhelper

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

import rescala.engine.Engine
import rescala.graph.{ATicket, Reactive}
import rescala.parrp.{Backoff, ParRP}
import rescala.propagation.Turn
import rescala.reactives.Signal
import rescala.twoversion.EngineImpl
import tests.rescala.concurrency.Spawn

trait PessimisticTestState {

  class PessimisticTestTurn extends ParRP(backoff = new Backoff(), None) {
    override def evaluate(r: Reactive[ParRP], ticket: ATicket[ParRP]): Unit = {
      while (Pessigen.syncStack.get() match {
        case stack@(set, bar) :: tail if set(r) =>
          bar.ready.countDown()
          bar.go.await()
          Pessigen.syncStack.compareAndSet(stack, tail)
          true
        case _ => false
      }) {}
      super.evaluate(r, ticket)
    }
  }

  case class Barrier(ready: CountDownLatch, go: CountDownLatch) {
    def await(): Unit = {
      ready.await(1, TimeUnit.SECONDS)
      go.countDown()
    }
  }

  object Pessigen extends EngineImpl[ParRP, PessimisticTestTurn]("Pessigen", new PessimisticTestTurn) {
    val syncStack: AtomicReference[List[(Set[Reactive], Barrier)]] = new AtomicReference(Nil)

    def clear(): Int = syncStack.getAndSet(Nil).size

    def sync(reactives: Reactive*): Unit = {
      val bar = syncm(reactives: _*)
      Spawn(bar.await())
    }

    def syncm(reactives: Reactive*): Barrier = {
      val ready = new CountDownLatch(reactives.size)
      val go = new CountDownLatch(1)
      val syncSet = reactives.toSet
      val bar = Barrier(ready, go)
      syncStack.set(syncStack.get() :+ ((syncSet, bar)))
      bar
    }

  }

  implicit def engine: Engine[ParRP, Turn[ParRP]] = Pessigen
  def unsafeNow[T](s: Signal[T, ParRP]): T = {
    engine.plan()(t => s.pulse(t.makeTicket()).get)
  }

}
