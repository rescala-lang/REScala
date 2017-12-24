package rescala.parrp

import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicInteger

import rescala.core.{ReSource, Reactive}
import rescala.locking._
import rescala.twoversion.TwoVersionEngineImpl

class ParallelLockSweep(backoff: Backoff, ex: Executor, engine: TwoVersionEngineImpl[LSStruct, ParallelLockSweep], priorTurn: Option[ParallelLockSweep]) extends LockSweep(backoff, priorTurn) {

  private type TState = LSStruct

  val jobsRunning = new AtomicInteger(0)

  def propagateWhileNonEmpty(): Unit = {
    while (true) {
      val reactive = synchronized {
        if (queue.isEmpty) return
        queue.pop()
      }
      asyncEvaluate(reactive)
    }
  }

  override def propagationPhase(): Unit = {
    do {propagateWhileNonEmpty()} while (jobsRunning.get() > 0)
  }

  def asyncEvaluate(head: Reactive[TState]): Unit = {
    if (head.state.anyInputChanged != this) synchronized { done(head, hasChanged = false) }
    else {
      jobsRunning.incrementAndGet()
      ex.execute {
        new Runnable {
          override def run(): Unit = {
            engine.withTurn(ParallelLockSweep.this) {
              evaluate(head)
            }
            jobsRunning.decrementAndGet()
          }
        }
      }
    }
  }

  override def evaluate(head: Reactive[TState]): Unit = {
    val res = head.reevaluate(this, head.state.base(token), head.state.incoming(this))
    synchronized {
      res.commitDependencyDiff(this, head)
      if (head.state.isGlitchFreeReady) {
        // val outgoings = res.commitValueChange()
        if(res.valueChanged) writeState(head)(res.value)

        head.state.hasWritten = this

        // done(head, res.valueChanged, res.commitValueChange())
        done(head, res.valueChanged)
      }
    }
  }

  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: ReSource[TState]): Unit = synchronized(super.dynamicDependencyInteraction(dependency))

  override def acquireShared(reactive: ReSource[TState]): Key[LSInterTurn] = synchronized(super.acquireShared(reactive))
}

