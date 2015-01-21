package rescala.synchronization

import java.util.concurrent.ConcurrentLinkedQueue

import rescala.graph.Globals
import rescala.turns.Turn

import scala.annotation.tailrec

final class Key(val turn: Turn) {
  val id = Globals.nextID()
  override def toString: String = s"Key($id)"

  @volatile var keychain: Keychain = new Keychain(this)
  var isHead: Boolean = true

  def cycle() = {
    lockKeychain {
      keychain.keys = keychain.keys.filter(ne)
      transferAll(keychain.keys.head)
      keychain.keys = keychain.keys.enqueue(this)
    }
  }

  @tailrec
  def lockKeychain[R](f: => R): R = {
    val oldChain = keychain
    keychain.synchronized {
      if (oldChain == keychain) Some(f)
      else None
    } match {
      case None => lockKeychain(f)
      case Some(r) => r
    }
  }

  /** contains a list of all locks owned by us. */
  private[this] val heldLocks = new ConcurrentLinkedQueue[TurnLock]()

  def addLock(lock: TurnLock): Unit = {
    heldLocks.add(lock)
  }


  def transferAll(target: Key): Unit =
    while (!heldLocks.isEmpty) {
      val head = heldLocks.poll()
      head.synchronized {
        val owner = head.getOwner
        val realTarget = if (head.isShared) target else null
        if (owner eq this) {
          head.transfer(realTarget, this)
        }
        else assert(owner eq realTarget, s"transfer of $head from $this to $realTarget failed, because it was owned by $owner")
      }
    }

  /** release all locks we hold or transfer them to a waiting transaction if there is one
    * holds the master lock for request */
  def releaseAll(): Unit =
    lockKeychain {
      assert(keychain.isHead(this), s"tried to drop $this from $keychain but is not head! (${keychain.keys})")
      keychain.releaseHead()
    }

}
