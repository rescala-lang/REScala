package rescala.locking

import java.util

import rescala.graph.Globals

class Keychain[InterTurn](init: Key[InterTurn]) {

  val id = Globals.nextID()
  override def toString = s"Keychain($id)"

  /** synchronized on this */
  private val keys: util.ArrayDeque[Key[InterTurn]] = new util.ArrayDeque[Key[InterTurn]](2)
  keys.add(init)

  private var fallthrough: Map[Key[InterTurn], Int] = Map.empty
  def addFallthrough(key: Key[InterTurn], amount: Int = 1): Unit = synchronized { fallthrough = fallthrough.updated(key, fallthrough.getOrElse(key, 0) + amount) }
  def removeFallthrough(key: Key[InterTurn]): Unit = synchronized {
    val old = fallthrough.getOrElse(key, 0)
    if (old <= 1) fallthrough -= key
    else fallthrough = fallthrough.updated(key, old - 1)
  }

  def append(other: Keychain[InterTurn]): Unit = {
    assert(this ne other, s"tried to append $this to itself")
    assert(Thread.holdsLock(this), s"tried to append $this and $other without holding lock on $this")
    assert(Thread.holdsLock(other), s"tried to append $this and $other without holding lock on $other")
    val it = other.keys.iterator()
    while (it.hasNext()) {
      it.next().keychain = this
    }
    other.fallthrough.foreach { case (k, a) => addFallthrough(k, a) }
    keys.addAll(other.keys)
  }

  def release(key: Key[InterTurn]) = {
    assert(Thread.holdsLock(this), s"tried to release $key without holding $this")
    val head = keys.poll()
    assert(head eq key, s"tried to drop $key from $this but is not head! ($keys)")
    val locks = key.grabLocks()
    val lockIt = locks.keySet().iterator()
    if (keys.isEmpty) {
      while (lockIt.hasNext) lockIt.next().transfer(null, key)
    }
    else {
      val target = keys.peek()
      while (lockIt.hasNext) lockIt.next().transfer(target, key, transferWriteSet = fallthrough.nonEmpty)
      fallthrough -= target
      target.continue()
    }
    locks.clear()
  }

}

