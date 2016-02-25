package rescala.parrp

import java.util

import rescala.graph.Reactive

class LockSweep(backoff: Backoff) extends ParRP(backoff) {

  private type TState = ParRPStruct.type

  val sorted = new util.ArrayList[Reactive[TState]]

  /** lock all reactives reachable from the initial sources
    * retry when acquire returns false */
  override def preparationPhase(initialWrites: List[Reactive[TState]]): Unit = {
    val stack = new java.util.ArrayDeque[Reactive[TState]](10)
    initialWrites.foreach(stack.push)

    while (!stack.isEmpty) {
      val reactive = stack.pop()
      if (reactive.bud.lock.isOwner(key)) {
        sorted.add(reactive)
      }
      else {
        if (reactive.bud.lock.tryLock(key) eq key) {
          // we add the reactive again, so we can enter it into the `sorted` when the childern are processed
          stack.push(reactive)
          reactive.bud.outgoing.filterNot(_.bud.lock.isOwner(key)).foreach {stack.push}
        }
        else {
          key.lockKeychain {
            key.releaseAll()
            key.keychain = new Keychain(key)
          }
          backoff.backoff()
          stack.clear()
          sorted.clear()
          initialWrites.foreach(stack.push)
        }
      }
    }
  }

  override def propagationPhase(): Unit = {
    val it = sorted.listIterator(sorted.size())
    while (it.hasPrevious) it.previous().reevaluate()
  }

}
