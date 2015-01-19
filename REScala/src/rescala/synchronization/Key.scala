package rescala.synchronization

import java.util.concurrent.ConcurrentLinkedQueue

import rescala.graph.Globals
import rescala.turns.Turn

final class Key(val turn: Turn) {
  val id = Globals.nextID()
  override def toString: String = s"Key($id)"

  @volatile var keychain: Keychain = new Keychain(this)

  def cycle() = {
    lockChain {
      keychain.keys = keychain.keys.filter(ne)
      transferAll(keychain.keys.head)
      keychain.keys = keychain.keys ::: this :: Nil
    }
  }

  def lockChain[R](f: => R): R = {
    val oldChain = keychain
    keychain.synchronized {
      if (oldChain == keychain) Some(f)
      else None
    } match {
      case None => lockChain(f)
      case Some(r) => r
    }
  }

  /** contains a list of all locks owned by us. */
  private[this] val heldLocks = new ConcurrentLinkedQueue[TurnLock]()

  def addLock(lock: TurnLock): Unit = {
    heldLocks.add(lock)
  }

  /** we acquire the master lock for the target, because the target waits on one of the locks we transfer,
    * and it will wake up as soon as that one is unlocked and we do not want the target to start unlocking
    * or wait on someone else before we have everything transferred */
  def transferAll(target: Key): Unit =
    while (!heldLocks.isEmpty) {
      val head = heldLocks.poll()
      val owner = head.getOwner
      if (owner eq this) {
        head.transfer(target, this)
      }
      else assert(owner eq target, s"transfer of $head from $this to $target failed, because it was owned by $owner")
    }

  /** release all locks we hold or transfer them to a waiting transaction if there is one
    * holds the master lock for request */
  def releaseAll(): Unit =
    lockChain {
      assert(keychain.keys.head eq this, s"tried to drop $this from $keychain but is not head! (${keychain.keys})")
      keychain.keys = keychain.keys.tail
      keychain.keys match {
        case Nil => transferAll(null)
        case x :: xs => transferAll(x)
      }
    }

}
