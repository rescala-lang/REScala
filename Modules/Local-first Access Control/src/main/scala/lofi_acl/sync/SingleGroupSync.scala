package lofi_acl.sync

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import lofi_acl.crypto.{PrivateIdentity, PublicIdentity}
import lofi_acl.sync.SingleGroupSyncMessage.*
import rdts.base.{Bottom, Lattice}
import rdts.time.Dots

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.{Duration, MILLISECONDS, SECONDS}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

enum SingleGroupSyncMessage[RDT]:
  case AnnouncePeers(peers: Map[PublicIdentity, (String, Int)])
  case AddUsers(users: Set[PublicIdentity], dots: Dots, cc: Dots)
  case Delta(delta: RDT, dots: Dots, rdtCC: Dots, permCC: Dots)
  case Time(rdtTime: Dots, permTime: Dots)
  case RequestMissing(rdtMerged: Dots, rdtRx: Dots, permMerged: Dots, permRx: Dots)

class SingleGroupSync[RDT](
    private val localIdentity: PrivateIdentity,
    private val rdtReference: AtomicReference[(Dots, RDT)]
)(using
    lattice: Lattice[RDT],
    bottom: Bottom[RDT],
    msgJsonCode: JsonValueCodec[SingleGroupSyncMessage[RDT]]
) extends CausalityCheckingMessageHandler[SingleGroupSyncMessage[RDT]] {
  @volatile private var users: Set[PublicIdentity] = Set.empty
  @volatile private var localPermissionTime: Dots  = Dots.empty

  @volatile private var stopped = false

  private var remoteTimes: Map[PublicIdentity, Dots] = Map.empty

  // Only ever modified by a single thread
  @volatile private var rdtDeltaStore: DeltaStore[RDT] = DeltaStore.empty()
  @volatile private var receivedRdtDots: Dots          = Dots.empty

  @volatile private var permissionDeltaStore: DeltaStore[Set[PublicIdentity]] = DeltaStore.empty()
  @volatile private var receivedPermissionDots: Dots                          = Dots.empty

  @volatile private var maxReferencedRdtDots: Dots        = Dots.empty
  @volatile private var maxReferencedPermissionDots: Dots = Dots.empty

  private val connectionManager: ConnectionManager[SingleGroupSyncMessage[RDT]] =
    ConnectionManager(localIdentity, this)

  private val executor               = Executors.newCachedThreadPool()
  private given ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  /** Called by the connection manager. Thread safe. */
  override def receivedMessage(msg: SingleGroupSyncMessage[RDT], sender: PublicIdentity): Unit = {
    if users.contains(sender)
    then msgQueue.put((msg, sender))

    // If user is not known, we drop the message for now
    // TODO: Might be better to disconnect, or check for missing permission deltas with peers
  }

  override def newMessageWithMissingPredecessors(msg: SingleGroupSyncMessage[RDT], sender: PublicIdentity): Unit = {
    var rdtMissing  = Dots.empty
    var permMissing = Dots.empty

    msg match
      case Delta(delta, dots, rdtCC, permCC) =>
        receivedRdtDots = receivedRdtDots.union(dots)
        maxReferencedRdtDots = maxReferencedRdtDots.union(rdtCC).union(dots)
        maxReferencedPermissionDots = maxReferencedPermissionDots.union(permCC)
        rdtMissing = rdtCC
        permMissing = permCC
      case AddUsers(_, dots, cc) =>
        receivedPermissionDots = receivedPermissionDots.union(dots)
        maxReferencedPermissionDots = maxReferencedPermissionDots.union(dots).union(cc)
        permMissing = cc
      case _ => ???

    requestDeltasIfStillMissingAfterDelay(sender, rdtMissing, permMissing, Duration(1, SECONDS))
  }

  override def canHandleMessage(msg: SingleGroupSyncMessage[RDT]): Boolean = {
    msg match
      case Time(_, _)                => true
      case AddUsers(users, dots, cc) =>
        // TODO: It isn't really necessary to enforce causal consistency of group membership in this scenario
        localPermissionTime.contains(cc)
      case Delta(_, _, rdtCC, permCC) =>
        // Checking the delta stores is semantically equivalent to checking the AtomicRef[(Dots, _)], since we only store
        // merged deltas in store
        rdtDeltaStore.retrievableDots.contains(rdtCC) && localPermissionTime.contains(permCC)
      case AnnouncePeers(peers)       => true
      case RequestMissing(_, _, _, _) => true
  }

  override def handleMessage(msg: SingleGroupSyncMessage[RDT], sender: PublicIdentity): Boolean = {
    msg match
      case AnnouncePeers(peers) =>
        peers.foreach { case (user, (host, port)) =>
          connectionManager.connectToExpectingUserIfNoConnectionExists(host, port, user)
        }
        return false

      case AddUsers(newUsers, permissionDots, _) =>
        users = users.union(newUsers)
        val oldPermissionTime = localPermissionTime
        val newPermissionTime = oldPermissionTime.merge(permissionDots)
        localPermissionTime = newPermissionTime
        permissionDeltaStore = permissionDeltaStore.addDeltaIfNew(permissionDots, newUsers)
        return oldPermissionTime == newPermissionTime

      case Delta(delta, dots, _, _) => // rdtCC and permCC are already checked in canHandleMessage
        val (localRdtTime, _) = rdtReference.updateAndGet { case (oldRdtTime, oldRdt) =>
          (oldRdtTime.union(dots), lattice.merge(oldRdt, delta))
        }
        val oldDeltaStore = rdtDeltaStore
        rdtDeltaStore = oldDeltaStore.addDeltaIfNew(dots, delta)
        receivedRdtDots = receivedRdtDots.merge(dots)
        // Send remote information about which updates the remote might have missed
        connectionManager.send(sender, Time(localRdtTime, localPermissionTime))
        return oldDeltaStore.retrievableDots.contains(dots)

      case Time(remoteRdtTime, remotePermissionTime) =>
        maxReferencedRdtDots = maxReferencedRdtDots.union(remoteRdtTime)
        maxReferencedPermissionDots = maxReferencedPermissionDots.union(remotePermissionTime)

        if !(receivedRdtDots.contains(remoteRdtTime) && receivedPermissionDots.contains(remotePermissionTime)) then
          requestDeltasIfStillMissingAfterDelay(
            sender,
            receivedRdtDots,
            receivedPermissionDots,
            Duration(50, MILLISECONDS)
          )

        return false

      case RequestMissing(rdtMerged, rdtReceived, permMerged, permReceived) =>
        val rdtStore         = rdtDeltaStore
        val missingRdtDeltas = rdtStore.retrievableDots.subtract(rdtMerged.union(rdtReceived))
        rdtStore.readAvailableDeltasAsSingleDelta(missingRdtDeltas) match
          case Some((dots, delta)) =>
            connectionManager.send(
              sender,
              // Since we send everything that is missing on the remote but merged locally, we can send empty CC
              Delta(delta, dots, Dots.empty, Dots.empty)
            )
          case None =>

        val permissionStore         = permissionDeltaStore
        val missingPermissionDeltas = permissionStore.retrievableDots.subtract(permMerged.union(permReceived))
        permissionStore.readAvailableDeltasAsSingleDelta(missingPermissionDeltas) match
          case Some((dots, permDelta)) =>
            connectionManager.send(
              sender,
              // Since we send everything that is missing on the remote but merged locally, we can send empty CC
              AddUsers(permDelta, dots, Dots.empty)
            )
          case None =>
        return false
  }

  def requestDeltasIfStillMissingAfterDelay(
      remote: PublicIdentity,
      rdtDots: Dots,
      permDots: Dots,
      delay: Duration
  ): Unit = {
    val _ = Future {
      Thread.sleep(delay.toMillis)
      if !(receivedRdtDots.contains(rdtDots) && receivedPermissionDots.contains(permDots))
      then
        val _ = connectionManager.send(
          remote,
          RequestMissing(
            rdtDeltaStore.retrievableDots,
            receivedRdtDots,
            permissionDeltaStore.retrievableDots,
            receivedPermissionDots
          )
        )
    }
  }

  private val antiEntropyThread = executor.submit(new Runnable:
    private val rand = Random()
    override def run(): Unit =
      while (!stopped) {
        Thread.sleep(Duration(30, SECONDS).toMillis)

        val users                        = connectionManager.connectedUsers
        val userToRequestMissingDataFrom = users.iterator.drop(rand.nextInt(users.size)).nextOption()
        val userToSendTimeTo             = users.iterator.drop(rand.nextInt(users.size)).nextOption()

        if userToRequestMissingDataFrom.isDefined then
          val _ = connectionManager.send(
            userToRequestMissingDataFrom.get,
            RequestMissing(
              rdtDeltaStore.retrievableDots,
              receivedRdtDots,
              permissionDeltaStore.retrievableDots,
              receivedPermissionDots
            )
          )

        if userToSendTimeTo.isDefined then
          val _ = connectionManager.send(
            userToSendTimeTo.get,
            Time(rdtDeltaStore.retrievableDots, permissionDeltaStore.retrievableDots)
          )
      }
  )

}
