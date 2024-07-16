package reactives.locking

import java.util.concurrent.{Semaphore, ThreadLocalRandom}
import scala.collection.mutable.ArrayBuffer


object Key {
  def apply[IT](turn: IT): Key[IT] = {
    val id = ThreadLocalRandom.current().nextLong()
    val k = new Key(turn, id)
    k.keychain = Keychain(k, id)
    k
  }
}

final class Key[InterTurn] private(val turn: InterTurn, val id: Long) {

  /* access to this var is protected by the intrinsic lock of the current keychain,
   * i.e., the value pointed to by this reference … .
   * the lock keychain method ensures correct locking of this field */
  @volatile private[locking] var keychain: Keychain[InterTurn] = scala.compiletime.uninitialized

  override def toString: String = s"Key($id)"

  private val semaphore = new Semaphore(0)

  private[locking] def continue(): Unit = semaphore.release()
  private[locking] def await(): Unit    = semaphore.acquire()

  def lockKeychain[R](f: Keychain[InterTurn] => R): R = {
    while true do {
      val oldChain = keychain
      // we are worried that the value of keychain changes between the
      // call of the `keychain` accessor, and the call to synchronized
      // so we check that it is the same as before
      keychain.synchronized {
        if oldChain eq keychain then return f(oldChain)
      }
    }
    throw new AssertionError("broke out of infinite loop")
  }

  /** contains a list of all locks owned by us. */
  private val heldLocks: ArrayBuffer[ReLock[InterTurn]]       = ArrayBuffer[ReLock[InterTurn]]()
  private[locking] def addLock(lock: ReLock[InterTurn]): Unit = heldLocks.synchronized { heldLocks += lock; () }
  private[locking] def grabLocks()                            = heldLocks.synchronized(heldLocks)

  /** release all locks we hold or transfer them to a waiting transaction if there is one
    * holds the master lock for request
    */
  def releaseAll(): Unit = lockKeychain { _.release(this) }

  def reset(): Unit =
    lockKeychain { kc =>
      kc.release(this)
      keychain = Keychain(this, ThreadLocalRandom.current().nextLong())
    }

}
