package reactives.locking

import java.util
import java.util.concurrent.ThreadLocalRandom

class Keychain[InterTurn](init: Key[InterTurn]) {

  val id                = ThreadLocalRandom.current().nextLong()
  override def toString = s"Keychain($id)"

  /** synchronized on this */
  private val keys: util.ArrayDeque[Key[InterTurn]] = new util.ArrayDeque[Key[InterTurn]](2)
  keys.add(init)

  /* fallthrough counts dynamic interactions:
   * every time a turn adds a new outgoing dependency to the reactive of another turn,
   * we count one fallthrough for that turn, remembering that the turn requires locks.
   * we do not know which locks, so at the end we just pass all if there is any fallthrough.
   * we count down one fallthrough, if an added dependency is later removed again. */
  private var fallthrough: Map[Key[InterTurn], Int] = Map.empty
  def addFallthrough(key: Key[InterTurn], amount: Int = 1): Unit =
    synchronized { fallthrough = fallthrough.updated(key, fallthrough.getOrElse(key, 0) + amount) }
  def removeFallthrough(key: Key[InterTurn]): Unit =
    synchronized {
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
    ()
  }

  def release(key: Key[InterTurn]): Unit = {
    assert(Thread.holdsLock(this), s"tried to release $key without holding $this")
    val head = keys.poll()
    assert(head eq key, s"tried to drop $key from $this but is not head! ($keys)")
    val locks = key.grabLocks()
    assert(locks.toSet.size == locks.size, s"duplicated locks detected")
    if (keys.isEmpty) {
      locks.foreach(_.transfer(null, key))
    } else {
      val target = keys.peek()
      locks.foreach(_.transfer(target, key, transferWriteSet = fallthrough.nonEmpty))
      fallthrough -= target
      target.continue()
    }
    locks.synchronized(locks.clear())
  }

}
