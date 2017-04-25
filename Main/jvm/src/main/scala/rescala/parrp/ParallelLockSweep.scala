package rescala.parrp

import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicInteger

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph._
import rescala.locking._
import rescala.twoversion.EngineImpl

class ParallelLockSweep(backoff: Backoff, ex: Executor, engine: EngineImpl[LSStruct, ParallelLockSweep], priorTurn: Option[ParallelLockSweep]) extends LockSweep(backoff, priorTurn) {

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
    if (head.state.anyInputChanged != this) done(head, hasChanged = false)
    else {
      val turn = this
      jobsRunning.incrementAndGet()
      ex.execute {
        new Runnable {
          override def run(): Unit = {
            engine.setCurrentTurn(Some(turn))
            evaluate(head)
            engine.setCurrentTurn(None)
            jobsRunning.decrementAndGet()
          }
        }
      }
    }
  }

  override def evaluate(head: Reactive[TState]): Unit = {
    val res = head.reevaluate(this)
    synchronized {
      res match {
        case Static(isChange, value) =>
          val hasChanged = isChange && value != head.state.base(token)
          if (hasChanged) writeState(head)(value)
          done(head, hasChanged)

        case res@Dynamic(isChange, value, deps) =>
          applyDiff(head, res.depDiff(head.state.incoming(this)))
          head.state.counter = recount(deps.iterator)

          if (head.state.counter == 0) {
            val hasChanged = isChange && value != head.state.base(token)
            if (hasChanged) writeState(head)(value)
            done(head, hasChanged)
          }

      }
    }
  }


  /** allow turn to handle dynamic access to reactives */
  override def dynamicDependencyInteraction(dependency: Reactive[TState]): Unit = synchronized(super.dynamicDependencyInteraction(dependency))


  override def acquireShared(reactive: Reactive[TState]): Key[LSInterTurn] = synchronized(super.acquireShared(reactive))


}

