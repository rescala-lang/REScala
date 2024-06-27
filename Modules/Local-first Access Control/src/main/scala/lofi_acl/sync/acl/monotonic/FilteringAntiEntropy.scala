package lofi_acl.sync.acl.monotonic

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import lofi_acl.access
import lofi_acl.access.Operation.WRITE
import lofi_acl.access.{Filter, Operation, PermissionTree}
import lofi_acl.crypto.PublicIdentity.toPublicIdentity
import lofi_acl.crypto.{Ed25519Util, PrivateIdentity, PublicIdentity}
import lofi_acl.sync.acl.monotonic.FilteringAntiEntropy.{PartialDelta, messageJsonCodec}
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.*
import lofi_acl.sync.{ConnectionManager, DeltaMapWithPrefix, JsoniterCodecs, MessageReceiver}
import rdts.base.Lattice
import rdts.time.{Dot, Dots}

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.Queue
import scala.util.Random

trait Sync[RDT] {
  def receivedDelta(dot: Dot, rdt: RDT): Unit
}

// Responsible for enforcing ACL
class FilteringAntiEntropy[RDT](
    localIdentity: PrivateIdentity,
    initialAclMessages: List[AddAclEntry[RDT]], // TODO: Merge initial acl messages
    initialRdtDeltas: DeltaMapWithPrefix[RDT],
    syncInstance: Sync[RDT]
)(using rdtCodec: JsonValueCodec[RDT], filter: Filter[RDT], rdtLattice: Lattice[RDT])
    extends MessageReceiver[MonotonicAclSyncMessage[RDT]] {
  private val connectionManager = ConnectionManager[MonotonicAclSyncMessage[RDT]](
    localIdentity,
    this
  )(using SignatureVerifyingMessageSerialization[RDT](localIdentity.getPublic, localIdentity.identityKey.getPrivate))

  private val localPublicId = localIdentity.getPublic

  // Access Control List ------
  private val aclReference: AtomicReference[(Dots, MonotonicAcl[RDT])] =
    AtomicReference((Dots.empty, MonotonicAcl.empty[RDT]))
  @volatile private var receivedAclDots: Dots                 = aclReference.get._1 // received != applied
  @volatile private var aclDeltas: Map[Dot, AddAclEntry[RDT]] = Map.empty
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
  private var aclMessageBacklog = Queue.empty[(AddAclEntry[RDT], PublicIdentity)]
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
    val permissions       = acl.write(localPublicId).intersect(acl.read.getOrElse(remote, PermissionTree.empty))
    outboundMessageLock.synchronized {
      localSendPermissions = localSendPermissions + (remote -> permissions)
      val _ = connectionManager.send(remote, PermissionsInUse(aclVersion, permissions))
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

  @volatile private var stopped = false
  def setStopped(): Unit        = stopped = true

  def run(): Unit = {
    while !stopped do {
      try {
        val (msg, sender) = msgQueue.take()

        // Process message immediately or backlog it if not processable
        handleMessage(msg, sender)

        // If we processed an ACLEntry, maybe we need now can process backlogged messages
        if msg.isInstanceOf[AddAclEntry[RDT]] // We don't consider causal dependencies between deltas
        then processBacklog()
      } catch
        case e: InterruptedException =>
    }
  }

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
        val newPeers = peers.removedAll(connectionManager.connectedUsers)
        newPeers.foreach { case (user, (host, port)) =>
          connectionManager.connectToExpectingUserIfNoConnectionExists(host, port, user)
        }

      case aclMsg @ AddAclEntry(_, _, _, _, cc, _) =>
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
    while {
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

  private def updateAcl(aclDelta: AddAclEntry[RDT]): Unit = aclDelta match
    // TODO: Root of trust
    case AddAclEntry(principal, realm, operation, dot, _, _) => {
      var updateValidAndNew = false
      // Update ACL
      val (_, acl) = aclReference.updateAndGet {
        case (dots, acl) =>
          acl.addPermissionIfAllowed(principal, PublicIdentity.fromUid(dot.place), realm, operation) match
            case Some(updatedAcl) =>
              updateValidAndNew = dots.contains(dot) // Check if update is new
              (dots.add(dot), updatedAcl)
            case None =>
              updateValidAndNew = false // Invalid update, don't store and don't apply!
              (dots, acl)
      }

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
        else ??? // TODO: Move invalidated deltas to partial delta store
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
          // Execute ever 0.5 to 1.5 seconds, avoiding synchronization of these requests among replicas.
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

  def newPeers(peers: Map[PublicIdentity, (String, Int)]): Unit = {
    receivedMessage(AnnouncePeers(peers), localPublicId)
  }

  def mutateRdt(dot: Dot, delta: RDT): Unit = {
    require(!rdtDeltas.allDots.contains(dot))
    val (aclDots, acl) = aclReference.get()
    val filteredDelta  = filter.filter(delta, acl.write(localPublicId))
    syncInstance.receivedDelta(dot, filteredDelta)
    val deltaMsg: Delta[RDT] = Delta(filteredDelta, dot, aclDots)
    broadcastFiltered(deltaMsg)
  }

  def grantPermission(aclDot: Dot, affectedUser: PublicIdentity, realm: PermissionTree, operation: Operation): Unit = {
    val (aclDots, acl) = aclReference.get()
    require(!aclDots.contains(aclDot))

    operation match
      case Operation.READ  => require(realm <= acl.read(localPublicId))
      case Operation.WRITE => require(realm <= acl.write(localPublicId))

    val addAclMsg: AddAclEntry[RDT] = {
      val unsignedMsg: AddAclEntry[RDT] =
        AddAclEntry(affectedUser, realm, operation, aclDot, aclReference.get()._1, null)
      val encodedMsg = writeToArray(unsignedMsg)(using messageJsonCodec)
      val signature  = Ed25519Util.sign(encodedMsg, localIdentity.identityKey.getPrivate)
      unsignedMsg.copy(signature = signature)
    }

    receivedMessage(addAclMsg, localPublicId)
    val _ = connectionManager.broadcast(addAclMsg)
  }
}

object FilteringAntiEntropy {
  import JsoniterCodecs.{pubIdentityKeyCodec, uidKeyCodec}

  given messageJsonCodec[RDT: JsonValueCodec]: JsonValueCodec[MonotonicAclSyncMessage[RDT]] = JsonCodecMaker.make(
    CodecMakerConfig.withAllowRecursiveTypes(true) // Required for PermissionTree
  )

  case class PartialDelta[RDT](delta: RDT, includedParts: PermissionTree, requiredPermissions: PermissionTree)
}
