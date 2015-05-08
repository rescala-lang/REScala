package rescala.turns

import rescala.graph.{Committable, Reactive}
import java.util.concurrent.Semaphore
import rescala.graph.Globals
import rescala.synchronization.Keychain
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import rescala.synchronization.TurnLock

/**
 * The engine that schedules the (glitch-free) evaluation
 * of the nodes in the dependency graph.
 */
trait Turn {

  /** returns the engine of this turn */
  def engine: Engine[Turn]

  /** allow turn to handle dynamic access to reactives */
  def accessDynamic(dependency: Reactive): Unit

  /** admits a new source change */
  def admit(reactive: Reactive): Unit

  /** removes a reactive from evaluation */
  def forget(reactive: Reactive): Unit

  /** called when a new reactive is created and registered into the network
    * subclasses are expected to register the reactive with its dependencies
    * and calculate the correct level */
  def create[T <: Reactive](dependencies: Set[Reactive], dynamic: Boolean = false)(f: => T): T

  /** adds a dependency */
  def register(sink: Reactive)(source: Reactive): Unit

  /** removes a dependency */
  def unregister(sink: Reactive)(source: Reactive): Unit

  /** install a new commit handler */
  def schedule(committable: Committable): Unit

  /** plan a new after commit handler. this still runs before releasing locks */
  def observe(f: => Unit): Unit
  
  def >(other: Turn ) = other == this
  
   val id = Globals.nextID()
  override def toString: String = s"Key($id)"

  @volatile var keychain: Keychain = new Keychain(this)

  private[this] val semaphore = new Semaphore(0)

  def continue(): Unit = semaphore.release()
  def await(): Unit = semaphore.acquire()


  def lockKeychain[R](f: => R): R = {
    @tailrec def loop(): R = {
      val oldChain = keychain
      keychain.synchronized {
        if (oldChain eq keychain) Some(f)
        else None
      } match {
        case None => loop()
        case Some(r) => r
      }
    }
    loop()
  }

  /** contains a list of all locks owned by us. */
  private[this] val heldLocks = new AtomicReference[List[TurnLock]](Nil)

 
  def addLock(lock: TurnLock): Unit = {
     @tailrec def addLockImpl(lock: TurnLock) : Unit = {
      val old = heldLocks.get()
      if (!heldLocks.compareAndSet(old, lock :: old)) addLockImpl(lock)
     }
     addLockImpl(lock)
  }

  def grabLocks(): List[TurnLock] = heldLocks.getAndSet(Nil)

  /** release all locks we hold or transfer them to a waiting transaction if there is one
    * holds the master lock for request */
  def releaseAll(): Unit = lockKeychain { keychain.release(this) }
}
