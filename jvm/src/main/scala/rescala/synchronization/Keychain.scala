package rescala.synchronization

import rescala.graph.Globals

import scala.collection.immutable.Queue

class Keychain(init: KeyImpl) {

  val id = Globals.nextID()
  override def toString = s"Keychain($id)"

  /** synchronized on this */
  private var keys: Queue[KeyImpl] = Queue(init)
  private var fallthrough: Map[KeyImpl, Int] = Map()
  def addFallthrough(key: KeyImpl, amount: Int = 1): Unit = synchronized { fallthrough = fallthrough.updated(key, fallthrough.getOrElse(key, 0) + amount) }
  def removeFallthrough(key: KeyImpl): Unit = synchronized {
    val old = fallthrough.getOrElse(key, 0)
    if (old <= 1) fallthrough -= key
    else fallthrough = fallthrough.updated(key, old - 1)
  }

  def append(other: Keychain): Unit = {
    assert(this ne other, s"tried to append $this to itself")
    assert(Thread.holdsLock(this), s"tried to append $this and $other without holding lock on $this")
    assert(Thread.holdsLock(other), s"tried to append $this and $other without holding lock on $other")
    other.keys.foreach { k =>
      k.keychain = this
    }
    other.fallthrough.foreach { case (k, a) => addFallthrough(k, a) }
    keys = keys.enqueue(other.keys)
  }

  def release(key: KeyImpl) = {
    assert(Thread.holdsLock(this), s"tried to release $key without holding $this")
    val (h, r) = keys.dequeue
    assert(h eq key, s"tried to drop $key from $this but is not head! ($keys)")
    keys = r
    val locks = key.grabLocks().distinct
    if (keys.isEmpty) locks.foreach(_.transfer(null, key))
    else {
      val target = keys.head
      locks.foreach(_.transfer(target, key, ignoreShared = fallthrough.nonEmpty))
      fallthrough -= target
      target.continue()
    }
  }

}
