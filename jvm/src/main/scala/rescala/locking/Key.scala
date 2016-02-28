package rescala.locking

import java.util.concurrent.{ConcurrentHashMap, Semaphore}

import rescala.graph.Globals

final class Key[InterTurn](val turn: InterTurn) {

  val id = Globals.nextID()
  override def toString: String = s"Key($id)"

  @volatile var keychain: Keychain[InterTurn] = new Keychain(this)

  private[this] val semaphore = new Semaphore(0)

  private[locking] def continue(): Unit = semaphore.release()
  def await(): Unit = semaphore.acquire()


  def lockKeychain[R](f: => R): R = {
    while (true) {
      val oldChain = keychain
      keychain.synchronized {
        if (oldChain eq keychain) return f
      }
    }
    throw new AssertionError("broke out of infinite loop")
  }

  /** contains a list of all locks owned by us. */
  private[this] val heldLocks: ConcurrentHashMap[TurnLock[InterTurn], Boolean] = new ConcurrentHashMap[TurnLock[InterTurn], Boolean]()

  private[locking] def addLock(lock: TurnLock[InterTurn]): Unit = heldLocks.put(lock, true)

  private[locking] def grabLocks() = heldLocks

  /** release all locks we hold or transfer them to a waiting transaction if there is one
    * holds the master lock for request */
  def releaseAll(): Unit = lockKeychain {keychain.release(this)}

}
