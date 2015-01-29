package rescala.synchronization

import rescala.graph.Reactive
import rescala.turns.Turn

trait InterturnDependencyChanges extends Turn {
  self: Prelock =>
  /** registering a dependency on a node we do not personally own does require some additional care.
    * we move responsibility to the commit phase */
  abstract override def register(sink: Reactive)(source: Reactive): Unit = {
    val owner = AcquireShared(source, key)
    if (owner ne key) {
      if (!source.outgoing.get.contains(sink)) {
        owner.turn.register(sink)(source)
        owner.turn.admit(sink)
        key.lockKeychain(key.keychain.addFallthrough(owner))
      }
    }
    else {
      super.register(sink)(source)
    }
  }

  /** this is for cases where we register and then unregister the same dependency in a single turn */
  abstract override def unregister(sink: Reactive)(source: Reactive): Unit = {
    val owner = AcquireShared(source, key)
    if (owner ne key) {
      owner.turn.forget(sink)
      owner.turn.unregister(sink)(source)
      key.lockKeychain(key.keychain.removeFallthrough(owner))
    }
    else super.unregister(sink)(source)
  }
}
