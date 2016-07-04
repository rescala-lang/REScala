package rescala.parrp

import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicInteger

import rescala.engines.JVMEngines
import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph._
import rescala.locking._

import scala.util.DynamicVariable

class ParallelLockSweep(backoff: Backoff, ex: Executor) extends LockSweep(backoff) {

  private type TState = LSStruct.type

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
    if (head.bud.anyInputChanged != this) done(head, hasChanged = false)
    else {
      val turn = this
      jobsRunning.incrementAndGet()
      ex.execute {
        new Runnable {
          override def run(): Unit = {
            JVMEngines.parallellocksweep.setCurrentTurn(Some(turn))
            evaluate(head)
            JVMEngines.parallellocksweep.setCurrentTurn(None)
            jobsRunning.decrementAndGet()
          }
        }
      }
    }
  }

  override def evaluate(head: Reactive[TState]): Unit = {
    val res = head.reevaluate()(this)
    synchronized {
      res match {
        case Static(hasChanged) => done(head, hasChanged)

        case Dynamic(hasChanged, diff) =>
          diff.removed foreach drop(head)
          diff.added foreach discover(head)
          head.bud.counter = recount(diff.novel)

          if (head.bud.counter == 0) done(head, hasChanged)

      }
    }
  }


  var collectedDependenciesLocal: DynamicVariable[List[Reactive[TState]]] = new DynamicVariable[List[Reactive[TState]]](Nil)

  override def collectDependencies[T](f: => T): (T, Set[Reactive[TState]]) = {
    collectedDependenciesLocal.withValue(Nil) {
      val sideEffectingEvaluationResult = f
      val newDependencies = collectedDependenciesLocal.value.toSet
      (sideEffectingEvaluationResult, newDependencies)

    }
  }
  override def useDependency(dependency: Reactive[TState]): Unit = collectedDependenciesLocal.value ::= dependency


  /** allow turn to handle dynamic access to reactives */
  override def dependencyInteraction(dependency: Reactive[TState]): Unit = synchronized(super.dependencyInteraction(dependency))


  override def acquireShared(reactive: Reactive[TState]): Key[LSInterTurn] = synchronized(super.acquireShared(reactive))


}

