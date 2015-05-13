package rescala.synchronization

import rescala.graph.Reactive
import rescala.turns.Turn

trait InterturnDependencyChanges extends Turn {
  self: Prelock =>
  /** registering a dependency on a node we do not personally own does require some additional care.
    * we let the other turn update the dependency and admit the dependent into the propagation queue
    * so that it gets updated when that turn continues
    * the responsibility for correcly passing the locks is moved to the commit phase */
  abstract override def register(sink: Reactive)(source: Reactive): Unit = {
    val owner = AcquireShared(source, this)
    if (owner ne this) {
      if (!source.outgoing.get.contains(sink)) {
        owner.register(sink)(source)
        owner.admit(sink)
        lockKeychain(this.keychain.addFallthrough(owner))
      }
    }
    else {
      super.register(sink)(source)
    }
  }

  /** this is for cases where we register and then unregister the same dependency in a single turn */
  abstract override def unregister(sink: Reactive)(source: Reactive): Unit = {
    val owner = AcquireShared(source, this)
    if (owner ne this) {
      owner.unregister(sink)(source)
      lockKeychain(keychain.removeFallthrough(owner))
      if (!sink.incoming.get(this).exists(_.lock.isOwner(owner))) owner.forget(sink)
    }
    else super.unregister(sink)(source)
  }
}
