package rescala.testhelper

import java.util.concurrent.CountDownLatch

import rescala.engine.TurnSource
import rescala.graph.Struct
import rescala.reactives.{Event, Signal}

class SynchronizedReevaluation {
  var latches: List[CountDownLatch] = Nil
  def reev[X](v1: X): X = {
    latches.foreach { _.countDown() }
    latches.foreach { _.await() }
    latches = Nil
    v1
  }
  def addSynchronizationPoint(latch: CountDownLatch): Unit = {
    latches ::= latch
  }
}

object SynchronizedReevaluation {
  def apply[A, S <: Struct](sig: Signal[A, S])(implicit turnSource: TurnSource[S]): (SynchronizedReevaluation, Signal[A, S]) = {
    val sync = new SynchronizedReevaluation
    (sync, sig.map(sync.reev))
  }
  def apply[A, S <: Struct](evt: Event[A, S])(implicit turnSource: TurnSource[S]): (SynchronizedReevaluation, Event[A, S]) = {
    val sync = new SynchronizedReevaluation
    (sync, evt.map(sync.reev))
  }

  def autoSyncNextReevaluation(syncs: SynchronizedReevaluation*): CountDownLatch = {
    val latch = manuallySyncNextReevaluation(syncs:_*)
    latch.countDown()
    latch
  }

  def manuallySyncNextReevaluation(syncs: SynchronizedReevaluation*): CountDownLatch = {
    val latch = new CountDownLatch(syncs.size + 1)
    syncs.foreach(_.addSynchronizationPoint(latch))
    latch
  }
}
