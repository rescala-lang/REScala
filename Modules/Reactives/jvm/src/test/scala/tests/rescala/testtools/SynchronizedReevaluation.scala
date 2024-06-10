package tests.rescala.testtools

import reactives.core.CreationTicket
import reactives.SelectedScheduler.State

import java.util.concurrent.ForkJoinPool.ManagedBlocker
import java.util.concurrent.{CountDownLatch, ForkJoinPool}

class SynchronizedReevaluation extends ManagedBlocker {
  var latches: List[CountDownLatch]  = Nil
  var notifies: List[CountDownLatch] = Nil
  def reev[X](v1: X): X = {
    latches.foreach { _.countDown() }
    // notify after latches so tests can assert latch counts correctly
    notifies.foreach { _.countDown() }
    ForkJoinPool.managedBlock(this)
    latches = Nil
    notifies = Nil
    v1
  }

  override def isReleasable: Boolean = !latches.exists(_.getCount > 0)

  override def block(): Boolean = {
    latches.foreach { _.await() }
    true
  }

  def addSynchronizationPoint(latch: CountDownLatch): Unit = {
    latches ::= latch
  }

  def addNotifyPoint(latch: CountDownLatch): Unit = {
    notifies ::= latch
  }
}

class SynchronizedReevaluationApi(val api: reactives.default.type) {
  import api.*

  def SynchronizedReevaluation[A](sig: Signal[A])(using
      turnSource: CreationTicket[State]
  ): (SynchronizedReevaluation, Signal[A]) = {
    val sync = new SynchronizedReevaluation
    (sync, sig.map(sync.reev))
  }
  def SynchronizedReevaluation[A](evt: Event[A])(using
      turnSource: CreationTicket[State]
  ): (SynchronizedReevaluation, Event[A]) = {
    val sync = new SynchronizedReevaluation
    (sync, evt.map(sync.reev))
  }

  def autoSyncNextReevaluation(syncs: SynchronizedReevaluation*): CountDownLatch = {
    val latch = manuallySyncNextReevaluation(syncs*)
    latch.countDown()
    latch
  }

  def manuallySyncNextReevaluation(syncs: SynchronizedReevaluation*): CountDownLatch = {
    val latch = new CountDownLatch(syncs.size + 1)
    syncs.foreach(_.addSynchronizationPoint(latch))
    latch
  }

  def notifyOnceReached(syncs: SynchronizedReevaluation*): CountDownLatch = {
    val latch = new CountDownLatch(syncs.size)
    syncs.foreach(_.addNotifyPoint(latch))
    latch
  }
}
