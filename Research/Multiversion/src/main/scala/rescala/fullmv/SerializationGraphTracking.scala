package rescala.fullmv

import scala.annotation.tailrec
import scala.collection.mutable
import java.rmi.server.UnicastRemoteObject
import java.util.UUID
import java.io.ObjectStreamException
import java.rmi.RemoteException

trait HostCommunication extends java.rmi.Remote {
  // union find stuff
  /**
    * One-element trees are defined to have a rank of zero, and whenever two trees of the same rank r are united, the rank of the result is r+1.
    */
  @throws[RemoteException]
  def rank(node: Transaction): Int
  @throws[RemoteException]
  def find(node: Transaction): Transaction
  @throws[RemoteException]
  def union(node: Transaction, other: Transaction): Transaction
  @throws[RemoteException]
  def subordinate(node: Transaction, newParent: Transaction): Unit
  @throws[RemoteException]
  def lock(node: Transaction): Transaction
  @throws[RemoteException]
  def tryLock0(node: Transaction): (Transaction, Boolean)
  @throws[RemoteException]
  def unlock(node: Transaction): Transaction

  // ssg stuff
  @throws[RemoteException]
  def newRemote(node: Transaction, host: Host): (TransactionPhase, Set[Transaction])
  @throws[RemoteException]
  def receiveNewTransactionPhase(node: Transaction, newPhase: TransactionPhase): Unit
  @throws[RemoteException]
  def distributeNewSuccessors(successors: Map[Transaction, Set[Transaction]], except: Host): Unit
  @throws[RemoteException]
  def receiveAdditionalSuccessors(successors: Map[Transaction, Set[Transaction]]): Unit
}
trait Host extends HostCommunication with Serializable {
  val id: UUID
}

class RemoteHost(override val id: UUID, val proxy: HostCommunication) extends Host {
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
  override def distributeNewSuccessors(successors: Map[Transaction, Set[Transaction]], except: Host): Unit = proxy.distributeNewSuccessors(successors, except)
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

  type NewSuccessorsBroadcastBuffer = mutable.Map[Transaction, Set[Transaction]]
  def newBuffer(): NewSuccessorsBroadcastBuffer = mutable.Map().withDefaultValue(Set())

  private object remoteReceiver extends UnicastRemoteObject with HostCommunication {
    override def rank(node: Transaction): Int = node.assertLocal.rank
    override def find(node: Transaction): Transaction = node.assertLocal.find()
    override def union(node: Transaction, other: Transaction): Transaction = node.assertLocal.union(other)
    override def subordinate(node: Transaction, newParent: Transaction): Unit = node.assertLocal.subordinate(newParent)
    override def lock(node: Transaction): Transaction = node.assertLocal.lock()
    override def tryLock0(node: Transaction): (Transaction, Boolean) = node.assertLocal.tryLock0()
    override def unlock(node: Transaction): Transaction = node.assertLocal.unlock()

    override def newRemote(node: Transaction, host: Host): (TransactionPhase, Set[Transaction]) = node.assertLocal.addSharedHost(host)
    override def receiveNewTransactionPhase(node: Transaction, newPhase: TransactionPhase): Unit = node.assertRemote.phase = newPhase
    override def distributeNewSuccessors(successors: Map[Transaction, Set[Transaction]], except: Host): Unit = {
      var unicasts = Map[Host, Map[Transaction, Set[Transaction]]]().withDefaultValue(Map())
      successors.foreach{ case tuple @ (node, newSuccessors) =>
        val mutableBuffer = newBuffer()
        val localNode = node.assertLocal
        localNode.addSuccessorsLocally(newSuccessors, mutableBuffer)
          (localNode.sharedOnHosts - except).foreach { host =>
            unicasts = unicasts + (host -> (unicasts(host) + tuple))
          }
      }
      unicasts.foreach{case (host, successors) =>
       host.receiveAdditionalSuccessors(successors)
      }
    }
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
  def replaceOrRegisterReceivedTransaction(transaction: Transaction): Transaction = {
    synchronized { transactionCache.get(transaction.id) match {
        case Some(existing) =>
          Left(existing)
        case None =>
          val remote = transaction.assertRemote
          registerTransaction(remote)
          Right(remote)
      }
    } match {
      case Left(existing) =>
        existing
      case Right(remote) =>
        remote.ingrain()
        remote
    }
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
  import Host.NewSuccessorsBroadcastBuffer
  var successors: Set[Transaction]
  def addSuccessorsLocally(newSuccessors: Set[Transaction], mutableBuffer: NewSuccessorsBroadcastBuffer): Unit = synchronized {
    val actuallyNewSuccessors = newSuccessors -- successors
    if(actuallyNewSuccessors.nonEmpty) {
      successors ++= actuallyNewSuccessors
      mutableBuffer += (this -> (mutableBuffer(this) ++ actuallyNewSuccessors))
    }
  }

  def searchClosure(target: Transaction, mutableBuffer: NewSuccessorsBroadcastBuffer): Boolean = {
    searchClosure0(target, Set(), mutableBuffer)._1
  }

  def searchClosure0(target: Transaction, visited: Set[Transaction], mutableBuffer: NewSuccessorsBroadcastBuffer): (Boolean, Set[Transaction]) = {
    val succs = successors -- visited
    val updatedVisited = visited ++ succs
    if (succs(target)) {
      (true, updatedVisited)
    } else {
      @tailrec def loop(iterator: Iterator[Transaction], visited: Set[Transaction]): (Boolean, Set[Transaction]) = {
        if (iterator.hasNext) {
          val successor = iterator.next
          val result @ (found, updatedVisited) = successor.searchClosure0(target, visited, mutableBuffer)
          addSuccessorsLocally(successor.successors, mutableBuffer)
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

  private def searchOrEstablishAndSendAndUnlock(against: Transaction, rootLocked: Transaction, mutableBuffer: NewSuccessorsBroadcastBuffer): OrderResult = {
    val resultLockedSearch = searchClosures(against, mutableBuffer)
    if (resultLockedSearch.isDefined) {
      rootLocked.unlock()
      // can unlock before sending as new edges are only transitive shortcuts that can be written without locks
      sendBuffer(mutableBuffer)
      resultLockedSearch.get
    } else {
      establishAndSendAndUnlock(against, rootLocked, mutableBuffer)
    }
  }
  def ensureAndGetOrder(against: Transaction): OrderResult = {
    if (this == against) throw new IllegalArgumentException("Cannot order against self!")
    if (phase() == Completed) throw new IllegalStateException("Completed transactions may no longer perform operations");
    if (against.phase() == Completed) {
      OtherFirst
    } else {
      if (find() == against.find()) {
        val mutableBuffer = Host.newBuffer()
        val resultUnlockedSearch = searchClosures(against, mutableBuffer)
        if (resultUnlockedSearch.isDefined) {
          sendBuffer(mutableBuffer)
          resultUnlockedSearch.get
        } else {
          val rootLocked = lock()
          searchOrEstablishAndSendAndUnlock(against, rootLocked, mutableBuffer)
        }
      } else {
        @tailrec
        def retryLock(lockFirst: Transaction, lockSecond: Transaction, self: Transaction, against: Transaction): OrderResult = {
          val rootFirstLocked = lockFirst.lock()
          val rootSecond = lockSecond.find()
          if (rootFirstLocked == rootSecond) {
            searchOrEstablishAndSendAndUnlock(against, rootFirstLocked, Host.newBuffer())
          } else {
            val rootSecondLocked = rootSecond.tryLock
            if (rootSecondLocked.isDefined) {
              val rootLocked = rootFirstLocked.union(rootSecondLocked.get)
              establishAndSendAndUnlock(against, rootLocked, Host.newBuffer())
            } else {
              rootFirstLocked.unlock()
              retryLock(lockSecond, lockFirst, self, against)
            }
          }
        }
        retryLock(this, against, this, against)
      }
    }
  }
  private def searchClosures(against: Transaction, mutableBuffer: NewSuccessorsBroadcastBuffer): Option[OrderResult] = {
    val foundAgainst = searchClosure(against, mutableBuffer)
    if (foundAgainst) {
      Some(SelfFirst)
    } else {
      val foundThis = against.searchClosure(this, mutableBuffer)
      if (foundThis) {
        Some(OtherFirst)
      } else {
        None
      }
    }
  }
  private def establishAndSendAndUnlock(against: Transaction, rootLocked: Transaction, mutableBuffer: NewSuccessorsBroadcastBuffer): OrderResult = {
    if (phase() == Executing && against.phase() == Framing) {
      establishEdgeAndSendAndUnlock(rootLocked, this, against, mutableBuffer)
      SelfFirst
    } else {
      establishEdgeAndSendAndUnlock(rootLocked, against, this, mutableBuffer)
      OtherFirst
    }
  }
  private def establishEdgeAndSendAndUnlock(rootLocked: Transaction, from: Transaction, to: Transaction, mutableBuffer: NewSuccessorsBroadcastBuffer): Unit = {
    from.addSuccessorsLocally(to.successors + to, mutableBuffer)
    sendBuffer(mutableBuffer)
    rootLocked.unlock()
  }
  private def sendBuffer(mutableBuffer: NewSuccessorsBroadcastBuffer): Unit = {
    (Map() ++ mutableBuffer).groupBy(_._1.host).foreach {
      case (host, buffer) =>
        host.distributeNewSuccessors(buffer, Host.LOCALHOST)
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

  override def toString() = s"$data: LocalTransaction($id @ $host)"
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
  def ingrain(): Unit = synchronized {
    val (initPhase, initSuccessors) = host.newRemote(this, Host.LOCALHOST)
    phase = initPhase
    successors = initSuccessors
  }
  def receiveNewPhase(newPhase: TransactionPhase): Unit = synchronized {
    phase = newPhase
    if (newPhase == Completed) successors = null // breaking this on purpose; should not be accessed any more.
  }
  def receiveAdditionalSuccessors(newSuccessors: Set[Transaction]): Unit = synchronized { successors ++= newSuccessors }

  override def toString() = s"$data: RemoteTransaction($id @ $host)"

  override def find(): Transaction = host.find(this)
  override def union(other: Transaction): Transaction = host.union(this, other)
  override def rank(): Int = host.rank(this)
  override def subordinate(newParent: Transaction): Unit = host.subordinate(this, newParent)
  override def lock(): Transaction = host.lock(this)
  override def tryLock0(): (Transaction, Boolean) = host.tryLock0(this)
  override def unlock(): Transaction = host.unlock(this)
}
