package rescala.locking

import java.util.concurrent.Semaphore

import scala.collection.mutable.ArrayBuffer

final class Key[InterTurn](val turn: InterTurn) {

  /* access to this var is protected by the intrinsic lock of the current keychain,
   * i.e., the value pointed to by this reference … .
   * the lock keychain method ensures correct locking of this field */
  @volatile private[locking] var keychain: Keychain[InterTurn] = new Keychain(this)

  val id: Long                  = keychain.id
  override def toString: String = s"Key($id)"

  private[this] val semaphore = new Semaphore(0)

  private[locking] def continue(): Unit = semaphore.release()
  private[locking] def await(): Unit    = semaphore.acquire()

  def lockKeychain[R](f: Keychain[InterTurn] => R): R = {
    while (true) {
      val oldChain = keychain
      // we are worried that the value of keychain changes between the
      // call of the `keychain` accessor, and the call to synchronized
      // so we check that it is the same as before
      keychain.synchronized {
        if (oldChain eq keychain) return f(oldChain)
      }
    }
    throw new AssertionError("broke out of infinite loop")
  }

  /** contains a list of all locks owned by us. */
  private[this] val heldLocks: ArrayBuffer[ReLock[InterTurn]] = ArrayBuffer[ReLock[InterTurn]]()
  private[locking] def addLock(lock: ReLock[InterTurn]): Unit = heldLocks.synchronized { heldLocks += lock; () }
  private[locking] def grabLocks()                            = heldLocks.synchronized(heldLocks)

  /** release all locks we hold or transfer them to a waiting transaction if there is one
    * holds the master lock for request
    */
  def releaseAll(): Unit = lockKeychain { _.release(this) }

  def reset(): Unit =
    lockKeychain { kc =>
      kc.release(this)
      keychain = new Keychain(this)
    }

}
