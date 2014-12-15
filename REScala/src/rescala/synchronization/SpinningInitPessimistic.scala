package rescala.synchronization

import rescala.graph.Reactive
import rescala.propagation.LevelQueue

import scala.collection.SortedSet

class SpinningInitPessimistic extends Pessimistic {
  /** lock all reactives reachable from the initial sources */
  override def lockReachable(initialWrites: List[Reactive]): Unit = {
    lazy val lq = new LevelQueue(evaluate)

    def evaluate(reactive: Reactive): Unit = {
      if (acquireWrite(reactive)) reactive.dependants.get.foreach(lq.enqueue(-42))
      else {
        lq.clear()
        initialWrites.foreach(lq.enqueue(-42))
      }
    }
    initialWrites.foreach(lq.enqueue(-42))
    lq.evaluateQueue()
  }

  def acquireWrite(reactive: Reactive): Boolean =
    if (reactive.lock.tryLock(key) eq key) true
    else {
      key.withMaster { key.releaseAll() }
      false
    }

}
