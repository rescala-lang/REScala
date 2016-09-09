package rescala.fullmv

import scala.annotation.tailrec
import java.rmi.server.UnicastRemoteObject
import java.util.UUID
import java.io.ObjectStreamException

@remote trait HostCommunication {
  // union find stuff
  def rank(node: Transaction): Int
  def find(node: Transaction): Transaction
  def union(node: Transaction, other: Transaction): Transaction
  def subordinate(node: Transaction, newParent: Transaction): Unit
  def lock(node: Transaction): Transaction
  def tryLock0(node: Transaction): (Transaction, Boolean)
  def unlock(node: Transaction): Transaction

  // ssg stuff
  def newRemote(node: Transaction, host: Host): (TransactionPhase, Set[Transaction])
  def receiveNewTransactionPhase(node: Transaction, newPhase: TransactionPhase): Unit
  def sendAdditionalSuccessors(node: Transaction, newSuccessors: Set[Transaction]): Unit
  def receiveAdditionalSuccessors(successors: Map[Transaction, Set[Transaction]]): Unit
}
trait Host extends HostCommunication {
  val id: UUID
}

case class RemoteHost(override val id: UUID, val proxy: HostCommunication) extends Host {
  @throws[ObjectStreamException] protected def readResolve(): Any = Host.replaceOrRegisterReceivedHost(this)

  override def rank(node: Transaction): Int = proxy.rank(node)
  override def find(node: Transaction): Transaction = proxy.find(node)
  override def union(node: Transaction, other: Transaction): Transaction = proxy.union(node, other)
  override def subordinate(node: Transaction, newParent: Transaction): Unit = proxy.subordinate(node, newParent)
  override def lock(node: Transaction): Transaction = proxy.lock(node)
  override def tryLock0(node: Transaction): (Transaction, Boolean) = proxy.tryLock0(node)
  override def unlock(node: Transaction): Transaction = proxy.unlock(node)

  override def newRemote(node: Transaction, host: Host): (TransactionPhase, Set[Transaction]) = proxy.newRemote(node, host)
  override def receiveNewTransactionPhase(node: Transaction, newPhase: TransactionPhase): Unit = proxy.receiveNewTransactionPhase(node, newPhase)
  override def sendAdditionalSuccessors(node: Transaction, newSuccessors: Set[Transaction]): Unit = proxy.sendAdditionalSuccessors(node, newSuccessors)
  override def receiveAdditionalSuccessors(successors: Map[Transaction, Set[Transaction]]): Unit = proxy.receiveAdditionalSuccessors(successors)
}

object Host {
  var hostCache = Map[UUID, Host]()
  def replaceOrRegisterReceivedHost(host: Host): Host = synchronized {
    hostCache.getOrElse(host.id, {
      hostCache += host.id -> host
      host
    })
  }

  private val remoteReceiver = new UnicastRemoteObject with HostCommunication {
    override def rank(node: Transaction): Int = node.assertLocal.rank
    override def find(node: Transaction): Transaction = node.assertLocal.find()
    override def union(node: Transaction, other: Transaction): Transaction = node.assertLocal.union(other)
    override def subordinate(node: Transaction, newParent: Transaction): Unit = node.assertLocal.subordinate(newParent)
    override def lock(node: Transaction): Transaction = node.assertLocal.lock()
    override def tryLock0(node: Transaction): (Transaction, Boolean) = node.assertLocal.tryLock0()
    override def unlock(node: Transaction): Transaction = node.assertLocal.unlock()

    override def newRemote(node: Transaction, host: Host): (TransactionPhase, Set[Transaction]) = node.assertLocal.addSharedHost(host)
    override def receiveNewTransactionPhase(node: Transaction, newPhase: TransactionPhase): Unit = node.assertRemote.phase = newPhase
    override def sendAdditionalSuccessors(node: Transaction, newSuccessors: Set[Transaction]): Unit = node.assertLocal.addSuccessors(newSuccessors)
    override def receiveAdditionalSuccessors(successors: Map[Transaction, Set[Transaction]]): Unit = successors.foreach {
      case (node, newSuccessors) =>
        node.assertRemote.receiveAdditionalSuccessors(newSuccessors)
    }
  }
  val LOCALHOST: Host = new RemoteHost(UUID.randomUUID(), remoteReceiver);
  synchronized(hostCache += LOCALHOST.id -> LOCALHOST)

  def shutdown(force: Boolean): Unit = {
    UnicastRemoteObject.unexportObject(remoteReceiver, force)
  }

  var transactionCache = Map[UUID, Transaction]()
  def registerTransaction(transaction: Transaction): Host = synchronized {
    transactionCache += transaction.id -> transaction
    LOCALHOST
  }
  def replaceOrRegisterReceivedTransaction(transaction: Transaction): Transaction = synchronized {
    transactionCache.getOrElse(transaction.id, {
      val remote = transaction.assertRemote
      registerTransaction(remote)
      remote.ingrain()
      remote
    })
  }
}

trait Transaction extends Serializable {
  // remote stuff
  val id: UUID
  val host: Host
  @throws[ObjectStreamException] protected def readResolve(): Any = Host.replaceOrRegisterReceivedTransaction(this)
  def assertLocal: LocalTransaction
  def assertRemote: RemoteTransaction

  // "payload"?
  val data: String

  // union find locking stuff
  def find(): Transaction
  def union(other: Transaction): Transaction
  def rank(): Int
  def subordinate(newParent: Transaction): Unit
  def lock(): Transaction
  def tryLock(): Option[Transaction] = {
    val (newParent, success) = tryLock0()
    if (success) {
      Some(newParent)
    } else {
      None
    }
  }
  def tryLock0(): (Transaction, Boolean)
  def unlock(): Transaction

  // transaction and serialization order stuff
  def phase(): TransactionPhase
  def successors(): Set[Transaction]
  def searchClosure(target: Transaction): Boolean = {
    val (found, visited) = searchClosure0(target, Set())
    addSuccessors(visited)
    found
  }
  def searchClosure0(target: Transaction, visited: Set[Transaction]): (Boolean, Set[Transaction]) = {
    val succs = successors() -- visited
    val updatedVisited = visited ++ succs
    if (succs(target)) {
      (true, updatedVisited)
    } else {
      @tailrec def loop(iterator: Iterator[Transaction], visited: Set[Transaction]): (Boolean, Set[Transaction]) = {
        if (iterator.hasNext) {
          val successor = iterator.next
          val result @ (found, updatedVisited) = successor.searchClosure0(target, visited)
          addSuccessors(successor.successors())
          if (found) {
            result
          } else {
            loop(iterator, updatedVisited)
          }
        } else {
          (false, visited)
        }
      }
      loop(succs.iterator, updatedVisited)
    }
  }
  // TODO calls to this method should ideally be asynchronous and batched broadcasts instead of indivudal RMIs?
  def addSuccessors(newSuccessors: Set[Transaction]): Unit
  def ensureAndGetOrder(against: Transaction): OrderResult = {
    if (this == against) throw new IllegalArgumentException("Cannot order against self!")
    if (phase() == Completed) throw new IllegalStateException("Completed transactions may no longer perform operations");
    if (against.phase() == Completed) {
      OtherFirst
    } else {
      if (find() == against.find()) {
        searchClosures(against).getOrElse {
          val rootLocked = lock()
          val result = searchClosures(against).getOrElse {
            establishOrder(against)
          }
          rootLocked.unlock()
          result
        }
      } else {
        def retry(lockFirst: Transaction, lockSecond: Transaction, self: Transaction, against: Transaction): OrderResult = {
          val rootFirstLocked = lockFirst.lock()
          val rootSecond = lockSecond.find()
          if (rootFirstLocked == rootSecond) {
            val result = searchClosures(against).getOrElse {
              establishOrder(against)
            }
            rootFirstLocked.unlock()
            result
          } else {
            val rootSecondLocked = rootSecond.tryLock
            if (rootSecondLocked.isDefined) {
              val rootLocked = rootFirstLocked.union(rootSecondLocked.get)
              val result = establishOrder(against)
              rootLocked.unlock()
              result
            } else {
              rootFirstLocked.unlock()
              retry(lockSecond, lockFirst, self, against)
            }
          }
        }
        retry(this, against, this, against)
      }
    }
  }
  private def searchClosures(against: Transaction): Option[OrderResult] = {
    val foundAgainst = searchClosure(against)
    if (foundAgainst) {
      Some(SelfFirst)
    } else {
      val foundThis = against.searchClosure(this)
      if (foundThis) {
        Some(OtherFirst)
      } else {
        None
      }
    }
  }
  private def establishOrder(against: Transaction): OrderResult = {
    if (phase() == Executing && against.phase() == Framing) {
      against.addSuccessors(this.successors() + this)
      SelfFirst
    } else {
      addSuccessors(against.successors() + against)
      OtherFirst
    }
  }
}

sealed trait OrderResult
case object SelfFirst extends OrderResult
case object OtherFirst extends OrderResult

object Transaction {
  def apply(data: String): Transaction = new LocalTransaction(data)
}

sealed trait TransactionState
case object UnlockedHead extends TransactionState
case object LockedHead extends TransactionState
case class Subordinate(parent: Transaction) extends TransactionState

sealed trait TransactionPhase
case object Framing extends TransactionPhase
case object Executing extends TransactionPhase
case object Completed extends TransactionPhase

final class LocalTransaction(override val data: String) extends Transaction {
  override val id = UUID.randomUUID()
  override val host = Host.registerTransaction(this)
  def assertLocal: LocalTransaction = this
  def assertRemote: RemoteTransaction = throw new IllegalArgumentException(s"received remote broadcast for local node $this")

  var phase: TransactionPhase = Framing
  var sharedOnHosts = Set[Host]()
  def addSharedHost(host: Host): (TransactionPhase, Set[Transaction]) = synchronized {
    sharedOnHosts += host
    (phase, successors)
  }
  def beginTransaction(): Unit = phaseTransition(Framing, Executing)
  def endTransaction(): Unit = {
    phaseTransition(Executing, Completed)
    successors = null // breaking this on purpose; should not be accessed any more.
  }
  def phaseTransition(from: TransactionPhase, to: TransactionPhase): Unit = synchronized {
    if (phase != from) throw new IllegalStateException(s"Cannot transition from $from to $to due to current phase mismatch ($phase)")
    phase = to
    sharedOnHosts
  }.foreach { _.receiveNewTransactionPhase(this, to) }

  var successors = Set[Transaction]()
  def addSuccessors(newSuccessors: Set[Transaction]): Unit = synchronized {
    successors ++= newSuccessors
    sharedOnHosts
  }.foreach { _.receiveAdditionalSuccessors(Map(this -> newSuccessors)) }

  override def toString() = s"$data: NodeImpl($id @ $host)"
  var state: TransactionState = UnlockedHead
  var rank: Int = 0
  def subordinate(newParent: Transaction): Unit = {
    synchronized {
      state = Subordinate(newParent)
      notifyAll()
    }
  }
  def find(): Transaction = {
    state match {
      case Subordinate(parent) =>
        val newParent = parent.find()
        state = Subordinate(newParent)
        newParent
      case _ => this
    }
  }

  def union(other: Transaction): Transaction = {
    state match {
      case Subordinate(parent) =>
        find().union(other)
      case _ =>
        val otherRoot = other.find()
        if (this == otherRoot) {
          this
        } else {
          if (this.rank < otherRoot.rank()) {
            this.subordinate(otherRoot)
            otherRoot
          } else {
            otherRoot.subordinate(this)
            if (rank == otherRoot.rank()) rank += 1
            this
          }
        }
    }
  }

  private var locked: Boolean = false
  def lock(): Transaction = {
    synchronized {
      @tailrec def retry(): LockResult = {
        state match {
          case Subordinate(parent) =>
            HasParent(parent)
          case UnlockedHead =>
            state = LockedHead
            LockedSelf
          case LockedHead =>
            wait()
            retry()
        }
      }
      retry()
    } match {
      case LockedSelf =>
        this
      case HasParent(parent) =>
        val newParent = parent.lock()
        state = Subordinate(newParent)
        newParent
    }
  }

  def tryLock0(): (Transaction, Boolean) = {
    synchronized {
      state match {
        case Subordinate(parent) =>
          HasParent(parent)
        case UnlockedHead =>
          state = LockedHead
          LockedSelf
        case LockedHead =>
          SelfOccupied
      }
    } match {
      case LockedSelf =>
        (this, true)
      case SelfOccupied =>
        (this, false)
      case HasParent(parent) =>
        val pass @ (newParent, _) = parent.tryLock0()
        state = Subordinate(newParent)
        pass
    }
  }

  def unlock(): Transaction = {
    synchronized {
      state match {
        case Subordinate(parent) =>
          val newParent = parent.unlock()
          state = Subordinate(newParent)
          newParent
        case LockedHead =>
          state = UnlockedHead
          notify()
          this
        case UnlockedHead =>
          throw new IllegalStateException(s"Unlocking impossible because not locked: $this!")
      }
    }
  }

  lazy val remote = new RemoteTransaction(host, id, data)
  @throws[ObjectStreamException]
  private def writeReplace(): Any = remote
}

sealed trait LockResult
sealed trait TryLockResult
case object LockedSelf extends LockResult with TryLockResult
case class HasParent(parent: Transaction) extends LockResult with TryLockResult
case object SelfOccupied extends TryLockResult

class RemoteTransaction(override val host: Host, override val id: UUID, override val data: String) extends Transaction {
  override def assertLocal: LocalTransaction = throw new IllegalArgumentException(s"not a local node: $this")
  override def assertRemote: RemoteTransaction = this

  var phase: TransactionPhase = _
  var successors: Set[Transaction] = _
  def ingrain(): Unit = {
    val (initPhase, initSuccessors) = host.newRemote(this, Host.LOCALHOST)
    phase = initPhase
    successors = initSuccessors
  }
  def receiveNewPhase(newPhase: TransactionPhase): Unit = synchronized {
    phase = newPhase
    if (newPhase == Completed) successors = null // breaking this on purpose; should not be accessed any more.
  }
  def receiveAdditionalSuccessors(newSuccessors: Set[Transaction]): Unit = synchronized { successors ++= newSuccessors }
  override def addSuccessors(newSuccessors: Set[Transaction]): Unit = host.sendAdditionalSuccessors(this, newSuccessors)

  override def toString() = s"$data: NodeRemote($id @ $host)"

  override def find(): Transaction = host.find(this)
  override def union(other: Transaction): Transaction = host.union(this, other)
  override def rank(): Int = host.rank(this)
  override def subordinate(newParent: Transaction): Unit = host.subordinate(this, newParent)
  override def lock(): Transaction = host.lock(this)
  override def tryLock0(): (Transaction, Boolean) = host.tryLock0(this)
  override def unlock(): Transaction = host.unlock(this)
}