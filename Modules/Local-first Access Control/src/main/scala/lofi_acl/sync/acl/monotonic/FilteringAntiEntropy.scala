package lofi_acl.sync.acl.monotonic

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import lofi_acl.access
import lofi_acl.access.Operation.WRITE
import lofi_acl.access.{Filter, Operation, PermissionTree}
import lofi_acl.collections.DeltaMapWithPrefix
import lofi_acl.crypto.PublicIdentity.toPublicIdentity
import lofi_acl.crypto.{PrivateIdentity, PublicIdentity}
import lofi_acl.sync.acl.monotonic.FilteringAntiEntropy.PartialDelta
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.*
import lofi_acl.sync.{ConnectionManager, MessageReceiver}
import rdts.base.{Bottom, Lattice}
import rdts.time.{ArrayRanges, Dot, Dots}

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.Queue
import scala.util.Random

// Responsible for enforcing ACL
class FilteringAntiEntropy[RDT](
    localIdentity: PrivateIdentity,
    rootOfTrust: PublicIdentity,
    initialAclMessages: List[AclDelta[RDT]], // Signatures are assumed to have been validated already
    initialRdtDeltas: DeltaMapWithPrefix[RDT],
    syncInstance: Sync[RDT]
)(using
    rdtCodec: JsonValueCodec[RDT],
    filter: Filter[RDT],
    rdtLattice: Lattice[RDT],
    rdtBottom: Bottom[RDT],
    msgCodec: JsonValueCodec[MonotonicAclSyncMessage[RDT]]
) extends MessageReceiver[MonotonicAclSyncMessage[RDT]] {
  private val connectionManager = ConnectionManager[MonotonicAclSyncMessage[RDT]](localIdentity, this)(using
    SignatureVerifyingMessageSerialization[RDT](
      localIdentity.getPublic,
      localIdentity.identityKey.getPrivate
    )
  )

  private val localPublicId = localIdentity.getPublic

  // Only updated in message queue thread
  @volatile private var peerAddressCache = Set.empty[(PublicIdentity, (String, Int))]

  // Access Control List ------
  private val aclReference: AtomicReference[(Dots, MonotonicAcl[RDT])] = {
    AtomicReference((Dots.empty, MonotonicAcl[RDT](rootOfTrust, Map.empty, Map.empty)))
  }
  @volatile private var receivedAclDots: Dots = // received != applied
    initialAclMessages.foldLeft(Dots.empty)((dots, msg) => dots.add(msg.dot))
  @volatile private var aclDeltas: Map[Dot, AclDelta[RDT]] = initialAclMessages.map(msg => msg.dot -> msg).toMap
  @volatile private var sendersIntendedWritePermissions: Map[PublicIdentity, PermissionTree]  = Map.empty
  @volatile private var sendersEffectiveWritePermissions: Map[PublicIdentity, PermissionTree] = Map.empty
  // Lock for sendersIntendedWritePermissions and sendersEffectiveWritePermissions
  private val sendersWritePermissionsLock                                         = new {}
  @volatile private var localSendPermissions: Map[PublicIdentity, PermissionTree] = Map.empty
  // Lock for localSendPermissions. Required to ensure consistency between announcement of write permissions and used permissions in filter.
  private val outboundMessageLock = new {}

  // RDT ----------------------
  @volatile private var rdtDeltas: DeltaMapWithPrefix[RDT]             = initialRdtDeltas
  @volatile private var partialDeltaStore: Map[Dot, PartialDelta[RDT]] = Map.empty

  // Messages -----------------
  // Stores inbound messages
  val msgQueue: LinkedBlockingQueue[(MonotonicAclSyncMessage[RDT], PublicIdentity)] = LinkedBlockingQueue()
  // Stores deltas that couldn't be processed because of missing causal dependencies
  private var aclMessageBacklog: Queue[(AclDelta[RDT], PublicIdentity)] = {
    if initialAclMessages.isEmpty
    then Queue.empty
    else
      Queue.from(initialAclMessages.tail.map(msg => msg -> localPublicId)) // Initialize queue with initial acl messages
  }
  if initialAclMessages.nonEmpty
  then receivedMessage(initialAclMessages.head, localPublicId) // Also causes processing of aclMessageBacklog
  // Note that this stores the *intended*, not the effective permissions of the user at the time of sending!
  // We are taking the intersection of the actual permissions and the intended permissions at use time.
  private var deltaMessageBacklog = Queue.empty[(Delta[RDT], PublicIdentity, PermissionTree)]

  // Executed in threads from ConnectionManager, thread safe
  override def receivedMessage(msg: MonotonicAclSyncMessage[RDT], fromUser: PublicIdentity): Unit = {
    msgQueue.put(msg, fromUser)
  }

  // Executed in thread from ConnectionManager
  override def connectionEstablished(remote: PublicIdentity): Unit = {
    val (aclVersion, acl) = aclReference.get()
    val permissions =
      acl.write.getOrElse(localPublicId, PermissionTree.empty)
        .intersect(acl.read.getOrElse(remote, PermissionTree.empty))
    outboundMessageLock.synchronized {
      localSendPermissions = localSendPermissions + (remote -> permissions)
      val _ = connectionManager.send(remote, PermissionsInUse(aclVersion, permissions))
      // TODO: Maybe run after random time interval
      val _ = connectionManager.broadcast(AnnouncePeers(peerAddressCache))
    }
  }

  // Executed in thread from ConnectionManager
  override def connectionShutdown(remote: PublicIdentity): Unit = {
    // We never lock an outboundMessageLock inside of a sendersWritePermissionsLock
    outboundMessageLock.synchronized {
      sendersWritePermissionsLock.synchronized {
        localSendPermissions = localSendPermissions.removed(remote)
        sendersIntendedWritePermissions = sendersIntendedWritePermissions.removed(remote)
        sendersEffectiveWritePermissions = sendersEffectiveWritePermissions.removed(remote)
      }
    }
  }

  def newPeers(peers: Set[(PublicIdentity, (String, Int))]): Unit = {
    receivedMessage(AnnouncePeers(peers), localPublicId)
  }

  def mutateRdt(dot: Dot, delta: RDT): Unit = {
    require(!rdtDeltas.allDots.contains(dot))
    val (aclDots, acl)  = aclReference.get()
    val localWritePerms = acl.write.get(localPublicId)
    if localWritePerms.isEmpty
    then
      Console.err.println("Could not mutate RDT: missing permissions")
      return
    val filteredDelta        = filter.filter(delta, localWritePerms.get)
    val deltaMsg: Delta[RDT] = Delta(filteredDelta, dot, aclDots)
    msgQueue.put((deltaMsg, localPublicId))
    broadcastFiltered(deltaMsg)
  }

  def acl: MonotonicAcl[RDT] = aclReference.get()._2

  def grantPermission(aclDot: Dot, affectedUser: PublicIdentity, realm: PermissionTree, operation: Operation): Unit = {
    val (aclDots, acl) = aclReference.get()
    require(!aclDots.contains(aclDot))

    operation match
      case Operation.READ  => require(realm <= acl.read(localPublicId))
      case Operation.WRITE => require(realm <= acl.write(localPublicId))

    val addAclMsg: AclDelta[RDT] = {
      val unsignedDelta: AclDelta[RDT] =
        AclDelta(affectedUser, realm, operation, aclDot, aclReference.get()._1, null)
      MonotonicAcl.signDelta(unsignedDelta, localIdentity)
    }

    receivedMessage(addAclMsg, localPublicId)
    val _ = connectionManager.broadcast(addAclMsg)
  }

  @volatile private var stopped = false
  def stop(): Unit = {
    stopped = true
    connectionManager.shutdown()
  }

  def start(): Thread = {
    require(connectionManager.listenPort.isEmpty) // TODO: Allow restart?
    connectionManager.acceptIncomingConnections()
    val thread = Thread(() =>
      while !stopped do {
        try {
          val (msg, sender) = msgQueue.take()

          // Process message immediately or backlog it if not processable
          handleMessage(msg, sender)

          // If we processed an ACLEntry, maybe we need now can process backlogged messages
          if msg.isInstanceOf[AclDelta[RDT]] // We don't consider causal dependencies between deltas
          then processBacklog()
        } catch
          case e: InterruptedException =>
      }
    )
    thread.start()
    thread
  }

  def listenPort: Option[Int] = connectionManager.listenPort

  private def handleMessage(msg: MonotonicAclSyncMessage[RDT], sender: PublicIdentity): Unit = {
    // Messages are processed in the order in which they were sent and only one at a time.
    // This means, we can rely on the order of messages to derive the used permissions at time of sending (implies time
    // of filtering on remote).

    msg match
      case PermissionsInUse(minimumAclVersion, writePermission) =>
        val (aclDots, acl) = aclReference.get()
        if !aclDots.contains(minimumAclVersion)
        then
          // We know that the remote replica has missing ACL Deltas, so request immediately
          val _ = connectionManager.send(sender, RequestMissing(rdtDeltas.allDots, aclDots))

        sendersWritePermissionsLock.synchronized {
          sendersIntendedWritePermissions = sendersIntendedWritePermissions + (sender -> writePermission)
          sendersEffectiveWritePermissions = sendersEffectiveWritePermissions +
            (sender -> writePermission.intersect(acl.write.getOrElse(sender, PermissionTree.empty)))
        }

      case AnnouncePeers(peers) =>
        val newPeers = peers.filterNot((id, _) => connectionManager.connectedUsers.contains(id))
        peerAddressCache = peerAddressCache ++ peers
        newPeers.foreach { case (user, (host, port)) =>
          connectionManager.connectToExpectingUserIfNoConnectionExists(host, port, user)
        }

      case aclMsg @ AclDelta(_, _, _, _, cc, _) =>
        if aclReference.get()._1.contains(cc)
        then updateAcl(aclMsg)
        else aclMessageBacklog = aclMessageBacklog.appended(aclMsg -> sender)

      case delta @ Delta(_, _, aclCC) =>
        if aclReference.get()._1.contains(aclCC)
        then
          handlePartialDelta(delta, sendersEffectiveWritePermissions(sender))
          if partialDeltaStore.nonEmpty && msgQueue.isEmpty
          then antiEntropyThread.interrupt() // Request missing partial deltas immediately
        else
          deltaMessageBacklog =
            deltaMessageBacklog.appended((delta, sender, sendersIntendedWritePermissions(sender)))

      case RequestMissing(remoteRdtDots, remoteAclDots) =>
        val aclDots = aclReference.get()._1
        val rdtDots = rdtDeltas.deltaDots

        val missingAclDeltas = receivedAclDots.subtract(remoteAclDots).iterator.flatMap { dot => aclDeltas.get(dot) }
        val _ =
          connectionManager.sendMultiple(sender, missingAclDeltas.toArray*) // No need to lock or filter for ACL deltas

        val missingRdtDeltas = rdtDots.subtract(remoteRdtDots)
        val deltas =
          rdtDeltas.retrieveDeltas(missingRdtDeltas).map[Delta[RDT]]((dot, delta) => Delta(delta, dot, aclDots)).toArray
        val _ = disseminateFiltered(sender, deltas*)

        // Check if we're missing anything that the remote has
        if !(receivedAclDots.contains(remoteAclDots) && rdtDeltas.deltaDots.contains(remoteRdtDots))
        then
          // TODO: We could also request deltas from those replicas that can write missing RDT deltas
          val _ = connectionManager.send(sender, RequestMissing(rdtDots, aclDots))
  }

  private def processBacklog(): Unit = {
    // Process backlogged ACL entries
    while { // TODO: Could be optimized by processing them in topological order
      val aclDots = aclReference.get()._1
      val (processableAclDeltas, unprocessable) =
        aclMessageBacklog.partition((entry, _) => aclDots.contains(entry.cc))
      aclMessageBacklog = unprocessable
      processableAclDeltas.foreach((msg, sender) => handleMessage(msg, sender))
      processableAclDeltas.nonEmpty
    } do {}

    // Process backlogged deltas
    val (aclDots, acl)                    = aclReference.get()
    val partialDeltaStoreBeforeProcessing = partialDeltaStore
    val (processableDeltas, unprocessableDeltas) =
      deltaMessageBacklog.partition((delta, _, _) => aclDots.contains(delta.aclCC))
    deltaMessageBacklog = unprocessableDeltas
    processableDeltas.foreach { (delta, sender, intendedPermissions) =>
      val effectivePermissions = intendedPermissions.intersect(acl.write(sender))
      handlePartialDelta(delta, effectivePermissions)
    }
    if partialDeltaStore != partialDeltaStoreBeforeProcessing
    then antiEntropyThread.interrupt() // Request missing partial deltas immediately
  }

  private def handlePartialDelta(delta: Delta[RDT], sendersPermissions: PermissionTree): Unit = delta match
    // Causal dependency of delta on ACL already resolved
    case Delta(delta, dot, _) =>
      val existingPartialDelta = partialDeltaStore.get(dot)

      if existingPartialDelta.isEmpty then {
        val acl                     = aclReference.get()._2
        val authorsWritePermissions = acl.write(dot.place.toPublicIdentity)
        val requiredPermissions     = authorsWritePermissions.intersect(acl.read(localPublicId))
        if sendersPermissions <= requiredPermissions
        then // Immediately applicable
          rdtDeltas = rdtDeltas.addDelta(dot, delta)
          syncInstance.receivedDelta(dot, delta)
        else // delta is missing parts
          partialDeltaStore = partialDeltaStore + (dot -> PartialDelta(delta, sendersPermissions, requiredPermissions))
      } else {
        existingPartialDelta.get match
          case PartialDelta(storedDelta, includedParts, requiredPermissions) =>
            if includedParts.merge(sendersPermissions) <= requiredPermissions
            then // Existing partial delta merged with newly received partial delta is complete
              val completeDelta = delta.merge(storedDelta)
              rdtDeltas = rdtDeltas.addDelta(dot, completeDelta)
              syncInstance.receivedDelta(dot, completeDelta)
              partialDeltaStore = partialDeltaStore.removed(dot)
            else // Existing partial delta merged with newly received partial delta is not yet complete
              partialDeltaStore = partialDeltaStore + (dot -> PartialDelta(
                storedDelta.merge(delta),
                includedParts.merge(sendersPermissions),
                requiredPermissions
              ))
      }

  private def updateAcl(aclDelta: AclDelta[RDT]): Unit = aclDelta match
    case AclDelta(principal, realm, operation, dot, _, _) => {
      var updateValidAndNew = false
      // Update ACL
      val (_, oldAcl) = aclReference.getAndUpdate {
        case (dots, acl) =>
          acl.addPermissionIfAllowed(principal, PublicIdentity.fromUid(dot.place), realm, operation) match
            case Some(updatedAcl) =>
              updateValidAndNew = !dots.contains(dot) // Check if update is new
              (dots.add(dot), updatedAcl)
            case None =>
              updateValidAndNew = false // Invalid update, don't store and don't apply!
              (dots, acl)
      }
      val acl = aclReference.get()._2

      if !updateValidAndNew then return

      // Update effective writing permissions of affected remote
      sendersWritePermissionsLock.synchronized {
        sendersEffectiveWritePermissions = sendersEffectiveWritePermissions + {
          val intendedPermission = sendersIntendedWritePermissions.getOrElse(principal, PermissionTree.empty)
          val allowedPermission  = acl.write.getOrElse(principal, PermissionTree.empty)
          principal -> intendedPermission.intersect(allowedPermission)
        }
      }

      // Store ACL delta and update delta
      aclDeltas = aclDeltas + (dot -> aclDelta)
      receivedAclDots = receivedAclDots.add(dot)

      // Notify peers about updated sending permissions
      if principal == localPublicId
      then
        if operation == WRITE then updateLocalSendPermissions()
        else
          val oldReadPerms = oldAcl.read.getOrElse(localPublicId, PermissionTree.empty)
          val oldDeltas    = rdtDeltas // TODO: Does that really make sense to keep the prefix?
          rdtDeltas = DeltaMapWithPrefix(
            Dots.empty,
            Bottom[RDT].empty,
            rdtDeltas.deltaDots.copy(internal =
              Map(localPublicId.toUid -> rdtDeltas.deltaDots.internal.getOrElse(localPublicId.toUid, ArrayRanges.empty))
            ),
            rdtDeltas.deltas.filter(_._1.place.delegate == localPublicId.id)
          )
          val newReadPerms = acl.read.getOrElse(localPublicId, PermissionTree.empty)
          partialDeltaStore = partialDeltaStore ++ oldDeltas.deltas.map {
            (dot, delta) => dot -> PartialDelta(delta, oldReadPerms, newReadPerms)
          }
    }

  private def updateLocalSendPermissions(): Unit = {
    val (aclDots, acl) = aclReference.get()
    // We take the intersection of our write permission and the receivers read permissions
    val localWritePermissions = acl.write(localPublicId)
    outboundMessageLock.synchronized {
      val oldSendPermissions = localSendPermissions
      val newSendPermissions = oldSendPermissions.map { (remote, _) =>
        remote -> localWritePermissions.intersect(acl.read(remote))
      }
      localSendPermissions = newSendPermissions
      oldSendPermissions.foreach { (remote, oldPermissions) =>
        val newPermissions = newSendPermissions(remote)
        if oldPermissions != newPermissions
        then
          val _ = connectionManager.send(remote, PermissionsInUse(aclDots, newPermissions))
      }
    }
  }

  private def disseminateFiltered(receiver: PublicIdentity, deltas: Delta[RDT]*): Unit = {
    // Locking is required despite per-remote locking in ConnectionManager, since we need to ensure that we have the same
    // order of messages on the receiver as have locally.
    // This is because the PermissionsInUse message indicates that all messages afterwards are written with the specified
    // permission.
    outboundMessageLock.synchronized {
      localSendPermissions.get(receiver) match
        case Some(permissions) =>
          val _ = connectionManager.sendMultiple(
            receiver,
            deltas.map(delta => delta.copy(delta = filter.filter(delta.delta, permissions)))*
          )
        case None =>
    }
  }

  private def broadcastFiltered(delta: Delta[RDT]): Unit = {
    outboundMessageLock.synchronized {
      localSendPermissions.foreach { (receiver, permissions) =>
        val _ = connectionManager.send(receiver, delta.copy(delta = filter.filter(delta.delta, permissions)))
      }
    }
  }

  private val antiEntropyThread = new Thread {
    private val rand = Random()
    override def run(): Unit =
      while !stopped do {
        try {
          // Execute every 0.5 to 1.5 seconds, avoiding synchronization of these requests among replicas.
          // See: https://dl.acm.org/doi/10.1145/167954.166241
          val sleepAmount = 500 + rand.nextInt(1_000)
          Thread.sleep(sleepAmount)
        } catch {
          case _: InterruptedException => if stopped then return // Otherwise request missing deltas immediately
        }

        // If no partial deltas are stored locally, pick a random peer and request from them.
        // If there are partial deltas, use PartialReplicationPeerSubsetSolver to choose replicas to request from.

        val peers = sendersEffectiveWritePermissions.toArray
        if peers.nonEmpty
        then
          if partialDeltaStore.isEmpty
          then
            val _ = connectionManager.send(
              peers(rand.nextInt(peers.length))._1,
              RequestMissing(rdtDeltas.allDots, receivedAclDots)
            )
          else
            val partialDeltas = partialDeltaStore
            // TODO: Could be optimized to reduce number of replicas to contact and deltas to request at the cost of complexity
            // - If a replica won't provide progress, don't request specific dot
            //    - progress can be used for this: !(goal ⋂ writer_permission <= progress)
            // - Provide existing progress to PartialReplicationPeerSubsetSolver
            // - Provide union of requiredPermissions of partial deltas instead of local read permission
            val (replicasToRequestFrom, _) = PartialReplicationPeerSubsetSolver.randomSubsetThatAllowsReconstruction(
              sendersEffectiveWritePermissions,
              aclReference.get()._2.read.getOrElse(localPublicId, PermissionTree.empty)
            )
            val msg = RequestMissing[RDT](rdtDeltas.allDots, receivedAclDots)
            replicasToRequestFrom.foreach { remote =>
              val _ = connectionManager.send(remote, msg)
            }
      }
  }
  antiEntropyThread.start()
}

object FilteringAntiEntropy {
  case class PartialDelta[RDT](delta: RDT, includedParts: PermissionTree, requiredPermissions: PermissionTree)
}
