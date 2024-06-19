package lofi_acl.sync.acl.monotonic

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.access.{Filter, Operation, PermissionTree}
import lofi_acl.crypto.{Ed25519Util, PrivateIdentity, PublicIdentity}
import lofi_acl.sync.*
import lofi_acl.sync.JsoniterCodecs.{pubIdentityKeyCodec, uidKeyCodec}
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.*
import lofi_acl.{access, sync}
import rdts.base.{Bottom, Lattice}
import rdts.time.{Dot, Dots}

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.{Duration, MILLISECONDS, SECONDS}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

class SyncWithMonotonicAcl[RDT](
    private val localIdentity: PrivateIdentity,
    initialRdt: (Dots, RDT),
    initialAclMessages: List[AddAclEntry[RDT]]
)(using
    lattice: Lattice[RDT],
    bottom: Bottom[RDT],
    rdtJsonCode: JsonValueCodec[RDT],
    filter: Filter[RDT]
) extends CausalityCheckingMessageHandler[MonotonicAclSyncMessage[RDT]] {

  private val localPublicId = localIdentity.getPublic

  private val rdtReference: AtomicReference[(Dots, RDT)] = AtomicReference(initialRdt)
  private val lastLocalRdtDot: AtomicReference[Dot] =
    AtomicReference(initialRdt._1.max(localPublicId.toUid).getOrElse(Dot(localPublicId.toUid, -1)))

  private val aclReference: AtomicReference[(Dots, MonotonicAcl[RDT])] =
    AtomicReference((Dots.empty, MonotonicAcl.empty[RDT]))
  private val lastLocalAclDot: AtomicReference[Dot] = AtomicReference(
    initialAclMessages
      .filter(_.dot.place.delegate == localPublicId.id)
      .maxByOption(_.dot.time)
      .map(_.dot)
      .getOrElse(Dot(localPublicId.toUid, -1))
  )
  // TODO: All ACL updates need to be applied before creation of the connection manager
  initialAclMessages.foreach(msgQueue.put(_, localPublicId))

  @volatile private var stopped = false

  // Only ever modified by a single thread
  @volatile private var rdtDeltas: DeltaMapWithPrefix[RDT] =
    DeltaMapWithPrefix(initialRdt._1, initialRdt._2, Dots.empty, Map.empty)
  @volatile private var receivedRdtDots: Dots = initialRdt._1

  @volatile private var aclDeltas: Map[Dot, AddAclEntry[RDT]] = Map.empty
  @volatile private var receivedAclDots: Dots                 = aclReference.get._1

  private given messageJsonCodec: JsonValueCodec[MonotonicAclSyncMessage[RDT]] = JsonCodecMaker.make
  private given serializer: SignatureVerifyingMessageSerialization[RDT] =
    SignatureVerifyingMessageSerialization[RDT](
      localPublicId,
      localIdentity.identityKey.getPrivate
    )

  private val connectionManager: FilteringConnectionManager[RDT] = {
    val (aclDots, acl) = aclReference.get()
    FilteringConnectionManager[RDT](localIdentity, this, acl, aclDots)
  }

  private val executor               = Executors.newCachedThreadPool()
  private given ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  def grantPermissions(user: PublicIdentity, realm: String, typeOfPermission: Operation): Unit = {
    val dot = lastLocalAclDot.updateAndGet(dot => dot.advance)
    val addAclMsg: AddAclEntry[RDT] = {
      val unsignedMsg: AddAclEntry[RDT] =
        AddAclEntry(user, realm, typeOfPermission, dot, aclReference.get()._1, null)
      val encodedMsg = writeToArray(unsignedMsg)(using messageJsonCodec)
      val signature  = Ed25519Util.sign(encodedMsg, localIdentity.identityKey.getPrivate)
      unsignedMsg.copy(signature = signature)
    }
    receivedMessage(addAclMsg, localPublicId)
    val _ = connectionManager.broadcast(addAclMsg)
  }

  def mutateRdt(deltaMutator: RDT => RDT): Unit = {
    val dot         = lastLocalRdtDot.updateAndGet(dot => dot.advance)
    val (dots, rdt) = rdtReference.get()
    // Handing the message over to receivedMessage has the downside of the message not being merged immediately.
    val delta = Delta(delta = deltaMutator(rdt), dot, aclReference.get()._1)
    receivedMessage(delta, localIdentity.getPublic)
    val _ = connectionManager.broadcast(delta)
  }

  /** Thread safe. */
  override def receivedMessage(msg: MonotonicAclSyncMessage[RDT], sender: PublicIdentity): Unit = {
    if !aclReference.get()._2.containsPrincipal(sender) || sender == localPublicId then
      return // If user is not known, we drop the message for now

    // TODO: Might be better to disconnect

    // Signature is checked by serializer
    msgQueue.put((msg, sender))
  }

  override def newMessageWithMissingPredecessors(msg: MonotonicAclSyncMessage[RDT], sender: PublicIdentity): Unit = {
    msg match
      case Delta(_, dot, aclCC) =>
        receivedRdtDots = receivedRdtDots.add(dot)
        requestDeltasIfStillMissingAfterDelay(sender, Dots.empty, aclCC, Duration(1, SECONDS))
      case AddAclEntry(_, _, _, dot, aclCC, sig) =>
        receivedAclDots = receivedAclDots.add(dot)
        requestDeltasIfStillMissingAfterDelay(sender, Dots.empty, aclCC, Duration(1, SECONDS))
      case _ => throw new IllegalArgumentException(s"$msg can't have missing predecessors")
  }

  override def canHandleMessage(msg: MonotonicAclSyncMessage[RDT]): Boolean = {
    msg match
      case AddAclEntry(_, _, _, _, aclCC, _) => aclReference.get()._1.contains(aclCC)
      case Delta(_, _, aclCC)                => aclReference.get()._1.contains(aclCC)
      case _                                 => true
  }

  /** Handles the message and returns whether this message has changed the causal context. */
  override def handleMessage(msg: MonotonicAclSyncMessage[RDT], sender: PublicIdentity): Boolean = {
    msg match
      case AnnouncePeers(peers) =>
        peers.foreach { case (user, (host, port)) =>
          connectionManager.connectToExpectingUserIfNoConnectionExists(host, port, user)
        }
        return false

      case aclDelta @ AddAclEntry(principal, realm, operation, dot, _, _) =>
        // cc inclusion already checked in canHandleMessage
        var aclUpdated = false
        val (oldAclDots, _) = aclReference.getAndUpdate {
          case (dots, acl) =>
            val permissionTree = PermissionTree.fromPath(realm)
            acl.addPermissionIfAllowed(principal, PublicIdentity.fromUid(dot.place), permissionTree, operation) match
              case Some(updatedAcl) =>
                aclUpdated = true
                (dots.add(dot), updatedAcl)
              case None => (dots, acl)
        }
        if aclUpdated
        then
          ??? // TODO: Check if my permissions were updated -> remove invalidated deltas, request missing deltas
          aclDeltas = aclDeltas + (dot -> aclDelta)
          receivedAclDots = receivedAclDots.add(dot)
          return oldAclDots.contains(dot)
        else
          return false

      case Delta(delta, dot, _) =>
        // TODO: Move to receive
        rdtDeltas = rdtDeltas.addDelta(dot, delta)
        receivedRdtDots = receivedRdtDots.add(dot)

        // aclCC is already checked in canHandleMessage
        // delta is already filtered by FilteringMessageReceiver
        val (oldRdtDots, _) = rdtReference.getAndUpdate { case (oldRdtTime, oldRdt) =>
          (oldRdtTime.add(dot), lattice.merge(oldRdt, delta))
        }
        return oldRdtDots.contains(dot)

      case RequestMissing(remoteRdtDots, remoteAclDots) =>
        val missingRdtDeltas = rdtDeltas.deltaDots.subtract(remoteRdtDots)
        val deltas = rdtDeltas.retrieveDeltas(missingRdtDeltas).map((dot, delta) =>
          Delta(delta, dot, connectionManager.aclVersion)
        ).toArray
        connectionManager.sendMultiple(sender, deltas*)

        receivedAclDots.subtract(remoteAclDots).iterator.foreach { dot =>
          aclDeltas.get(dot) match
            case Some(aclDelta) => connectionManager.send(sender, aclDelta)
            case None           =>
        }

        if !(receivedRdtDots.contains(remoteRdtDots) && receivedAclDots.contains(remoteAclDots)) then
          requestDeltasIfStillMissingAfterDelay(
            sender,
            receivedRdtDots,
            receivedAclDots,
            Duration(50, MILLISECONDS)
          )
        return false
  }

  // TODO: Need to check that we efficiently request from those replicas that have sufficient permissions to reconstruct
  // delta
  private def requestDeltasIfStillMissingAfterDelay(
      remote: PublicIdentity,
      rdtDots: Dots,
      aclDots: Dots,
      delay: Duration
  ): Unit = {
    val _ = Future {
      Thread.sleep(delay.toMillis)
      if !(receivedRdtDots.contains(rdtDots) && receivedAclDots.contains(aclDots))
      then
        val _ = connectionManager.send(remote, RequestMissing(receivedRdtDots, receivedAclDots))
    }
  }

  private val antiEntropyThread = executor.submit(new Runnable:
    private val rand = Random()
    override def run(): Unit =
      while !stopped do {
        Thread.sleep(Duration(1, SECONDS).toMillis)

        val users                        = connectionManager.connectedUsers
        val userToRequestMissingDataFrom = users.iterator.drop(rand.nextInt(users.size)).nextOption()

        if userToRequestMissingDataFrom.isDefined then
          val _ = connectionManager.send(
            userToRequestMissingDataFrom.get,
            RequestMissing(receivedRdtDots, receivedAclDots)
          )
      }
  )

}
