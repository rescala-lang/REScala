package lofi_acl.sync.acl.bft

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import lofi_acl.access
import lofi_acl.access.{Filter, PermissionTree}
import lofi_acl.collections.DeltaMapWithPrefix
import lofi_acl.crypto.PublicIdentity.toPublicIdentity
import lofi_acl.crypto.{PrivateIdentity, PublicIdentity}
import lofi_acl.sync.acl.bft.BftAclOpGraph.{Delegation, EncodedDelegation, Signature}
import lofi_acl.sync.acl.bft.BftFilteringAntiEntropy.SyncMsg
import lofi_acl.sync.acl.bft.BftFilteringAntiEntropy.SyncMsg.*
import lofi_acl.sync.acl.monotonic.FilteringAntiEntropy.PartialDelta
import lofi_acl.sync.acl.monotonic.PartialReplicationPeerSubsetSolver
import lofi_acl.sync.{ConnectionManager, JsoniterCodecs, MessageReceiver, MessageSerialization}
import rdts.base.{Bottom, Lattice, Uid}
import rdts.time.{Dot, Dots}

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.Queue
import scala.util.Random

// Responsible for enforcing ACL
class BftFilteringAntiEntropy[RDT](
    localIdentity: PrivateIdentity,
    aclRoot: EncodedDelegation,
    syncInstance: SyncWithBftMonotonicAcl[RDT]
)(using
    rdtCodec: JsonValueCodec[RDT],
    filter: Filter[RDT],
    rdtLattice: Lattice[RDT],
    rdtBottom: Bottom[RDT],
) extends MessageReceiver[SyncMsg[RDT]] {
  private val connectionManager =
    ConnectionManager[SyncMsg[RDT]](localIdentity, this)(using MessageSerialization.derived[SyncMsg[RDT]])
  private val localPublicId = localIdentity.getPublic

  // Only updated in message queue thread
  private val peerAddressCache = AtomicReference(Set.empty[(PublicIdentity, (String, Int))])

  // RDT ----------------------
  @volatile private var rdtDeltas: DeltaMapWithPrefix[(RDT, Set[Signature])] = DeltaMapWithPrefix.empty
  @volatile private var partialDeltaStore: Map[Dot, PartialDelta[RDT]]       = Map.empty

  // Messages -----------------
  // Stores inbound messages
  val msgQueue: LinkedBlockingQueue[(SyncMsg[RDT], PublicIdentity)] = LinkedBlockingQueue()
  // Stores deltas that couldn't be processed because of missing causal dependencies
  private var aclMessageBacklog: Map[Signature, Delegation] = Map.empty
  private var deltaMessageBacklog                           = Queue.empty[(RdtDelta[RDT], PublicIdentity)]

  // Executed in threads from ConnectionManager, thread safe
  override def receivedMessage(msg: SyncMsg[RDT], fromUser: PublicIdentity): Unit = {
    msgQueue.put(msg, fromUser)
  }

  // Executed in thread from ConnectionManager
  override def connectionEstablished(remote: PublicIdentity): Unit = {
    val (aclOpGraph, acl) = syncInstance.currentAcl
    // TODO: It might make sense to tell the other side which document we're talking about. Otherwise they might try to
    //  sync unrelated op graphs
    val _ = connectionManager.send(remote, RequestMissingAcl(aclOpGraph.heads, Set.empty))
    val _ = connectionManager.broadcast(AnnouncePeers(peerAddressCache.get()))
  }

  // Executed in thread from ConnectionManager
  override def connectionShutdown(remote: PublicIdentity): Unit = {
    println(s"Disconnected from $remote")
  }

  def newPeers(peers: Set[(PublicIdentity, (String, Int))]): Unit = {
    receivedMessage(AnnouncePeers(peers), localPublicId)
  }

  def mutateRdt(dot: Dot, delta: RDT): Unit = {
    require(!rdtDeltas.allDots.contains(dot))
    val (aclOpGraph, acl) = syncInstance.currentAcl
    val localWritePerms   = acl.write.get(localPublicId)
    if localWritePerms.isEmpty
    then
      Console.err.println("Could not mutate RDT: missing write permissions")
      return
    val filteredDelta           = filter.filter(delta, localWritePerms.get)
    val deltaMsg: RdtDelta[RDT] = RdtDelta(dot, filteredDelta, aclOpGraph.heads, aclOpGraph.heads)
    msgQueue.put((deltaMsg, localPublicId))
    broadcastFiltered(acl, deltaMsg)
  }

  def broadcastAclDelegation(delegation: EncodedDelegation): Unit = {
    val _ = connectionManager.broadcast(AclDelta(delegation))
  }

  @volatile private var stopped = false
  def stop(): Unit = {
    stopped = true
    connectionManager.shutdown()
  }

  def start(): Thread = {
    require(connectionManager.listenPort.isEmpty) // TODO: Allow restart?
    connectionManager.acceptIncomingConnections()
    peerAddressCache.updateAndGet(cache => cache + (localPublicId -> ("localhost", connectionManager.listenPort.get)))
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

  private def handleMessage(msg: SyncMsg[RDT], sender: PublicIdentity): Unit = {
    msg match
      case deltaMsg @ RdtDelta(dot, delta, authorAclHeads, senderAclHeads) =>
        if !handlePartialDelta(deltaMsg, sender)
        then deltaMessageBacklog = deltaMessageBacklog.appended((deltaMsg, sender))

      case AclDelta(encodedDelegation) =>
        val missing = syncInstance.applyAclIfPossible(encodedDelegation)
        if missing.nonEmpty then {
          val _ = connectionManager.send(sender, RequestMissingAcl(syncInstance.currentAcl._1.heads, missing))
        } else {
          // TODO: Apply ACL deltas from backlog that can be handled after applying the received ACL delta
        }

      case RequestMissingAcl(heads: Set[Signature], knownMissing: Set[Signature]) =>
        // TODO: Save ACL peer state
        val opGraph = syncInstance.currentAcl._1
        val msgs = knownMissing
          .flatMap { sig => opGraph.ops.get(sig).map(delegation => delegation.encode(sig)) }
          .map[AclDelta[RDT]](delegation => AclDelta(delegation))
        val _ = connectionManager.sendMultiple(sender, msgs.toArray*)

        val missingLocally = heads.filterNot(opGraph.ops.contains)
        // TODO: Requesting should be deferred until after message queue is empty (+ some delay).
        if missingLocally.nonEmpty then {
          val _ = connectionManager.send(sender, RequestMissingAcl(syncInstance.currentAcl._1.heads, missingLocally))
        }

      case RequestMissingRdt(remoteRdtDots) =>
        // TODO: avoid possibility of interleaving with acl change
        val (aclOpGraph, acl) = syncInstance.currentAcl
        val rdtDeltaCopy      = rdtDeltas

        val rdtDots          = rdtDeltas.deltaDots
        val missingRdtDeltas = rdtDots.subtract(remoteRdtDots)
        val deltas =
          rdtDeltaCopy.retrieveDeltas(missingRdtDeltas).map[RdtDelta[RDT]] { case (dot, (delta, authorAcl)) =>
            RdtDelta(dot, delta, authorAcl, aclOpGraph.heads)
          }.toArray
        val _ = sendFiltered(sender, acl, deltas*)

        // Check if we're missing anything that the remote has
        if !rdtDeltas.deltaDots.contains(remoteRdtDots)
        then
          // TODO: We could also request deltas from those replicas that can write missing RDT deltas
          val _ = connectionManager.send(sender, RequestMissingRdt(rdtDots))

      case AnnouncePeers(peers) =>
        val newPeers      = peers.diff(peerAddressCache.get())
        val peerAddresses = peerAddressCache.updateAndGet(cache => cache ++ peers)
        if newPeers.nonEmpty then {
          val _ = connectionManager.broadcast(AnnouncePeers(peerAddresses))
        }
        newPeers.foreach { case (user, (host, port)) =>
          connectionManager.connectToExpectingUserIfNoConnectionExists(host, port, user)
        }
  }

  private def processBacklog(): Unit = {
    // Process backlogged rdt deltas
    val aclOpGraph                        = syncInstance.currentAcl._1
    val partialDeltaStoreBeforeProcessing = partialDeltaStore
    val (processableDeltas, unprocessableDeltas) = deltaMessageBacklog.partition((delta, _) =>
      delta.authorAclContext.union(delta.senderAclContext).forall(aclOpGraph.ops.contains)
    )
    deltaMessageBacklog = unprocessableDeltas
    processableDeltas.foreach { (delta, sender) =>
      handlePartialDelta(delta, sender)
    }
    // if partialDeltaStore != partialDeltaStoreBeforeProcessing
    // then antiEntropyThread.interrupt() // Request missing partial deltas immediately
  }

  // Returns false if not applicable because local ACL is not superset of author and senders ACL
  private def handlePartialDelta(unfilteredDelta: RdtDelta[RDT], sender: PublicIdentity): Boolean = {
    unfilteredDelta match
      case RdtDelta(dot, _, authorAclHeads, senderAclHeads) =>
        val (aclOpGraph, localAcl) = syncInstance.currentAcl
        if !authorAclHeads.union(senderAclHeads).forall(aclOpGraph.ops.contains) then return false

        val authorAcl =
          if authorAclHeads == aclOpGraph.heads
          then localAcl
          else aclOpGraph.reconstruct(authorAclHeads).get
        val senderAcl =
          if senderAclHeads == aclOpGraph.heads then localAcl
          else if senderAclHeads == authorAclHeads then authorAcl
          else aclOpGraph.reconstruct(senderAclHeads).get
        val effectivePermissions = authorAcl.write(dot.place.toPublicIdentity)
          .intersect(senderAcl.write(sender))
          .intersect(localAcl.read(localPublicId))

        if effectivePermissions.isEmpty then return true

        val delta = Filter[RDT].filter(unfilteredDelta.delta, effectivePermissions)

        val existingPartialDelta = partialDeltaStore.get(dot)
        if existingPartialDelta.isEmpty then {
          val requiredPermissions = authorAcl.write(dot.place.toPublicIdentity).intersect(localAcl.read(localPublicId))
          if requiredPermissions <= senderAcl.write(sender)
          then // Immediately applicable
            rdtDeltas = rdtDeltas.addDelta(dot, (delta, authorAclHeads))
            syncInstance.receivedDelta(dot, delta)
          else // delta is missing parts
            partialDeltaStore =
              partialDeltaStore + (dot -> PartialDelta(delta, effectivePermissions, requiredPermissions))
        } else {
          existingPartialDelta.get match
            case PartialDelta(storedDelta, includedParts, requiredPermissions) =>
              val combinedPermissions = includedParts.merge(effectivePermissions)
              if requiredPermissions <= combinedPermissions
              then // Existing partial delta merged with newly received partial delta is complete
                val completeDelta = delta.merge(storedDelta)
                rdtDeltas = rdtDeltas.addDelta(dot, (completeDelta, authorAclHeads))
                syncInstance.receivedDelta(dot, completeDelta)
                partialDeltaStore = partialDeltaStore.removed(dot)
              else // Existing partial delta merged with newly received partial delta is not yet complete
                partialDeltaStore = partialDeltaStore + (dot -> PartialDelta(
                  storedDelta.merge(delta),
                  combinedPermissions,
                  requiredPermissions
                ))
        }
        true
  }

  private def sendFiltered(receiver: PublicIdentity, acl: Acl, deltas: RdtDelta[RDT]*): Unit = {
    val permissions = acl.read(receiver).intersect(acl.write(localPublicId))
    if permissions.isEmpty then return
    val _ = connectionManager.sendMultiple(
      receiver,
      deltas.map(delta => delta.copy(delta = filter.filter(delta.delta, permissions)))*
    )
  }

  private def broadcastFiltered(acl: Acl, delta: RdtDelta[RDT]): Unit = {
    connectionManager.connectedUsers.foreach { receiver =>
      val permissions = acl.write.getOrElse(localPublicId, PermissionTree.empty).intersect(
        acl.read.getOrElse(receiver, PermissionTree.empty)
      )
      if !permissions.isEmpty then {
        val _ = connectionManager.send(receiver, delta.copy(delta = filter.filter(delta.delta, permissions)))
      }
    }
  }

  private val antiEntropyThread = new Thread {
    private val rand = Random()
    override def run(): Unit = {
      while !stopped do {
        try {
          // Execute every 1 to 3 seconds, avoiding synchronization of these requests among replicas.
          // See: https://dl.acm.org/doi/10.1145/167954.166241
          val sleepAmount = 1_000 + rand.nextInt(2_000)
          Thread.sleep(sleepAmount)
        } catch {
          case _: InterruptedException => if stopped then return // Otherwise request missing deltas immediately
        }

        // If no partial deltas are stored locally, pick a random peer and request from them.
        // If there are partial deltas, use PartialReplicationPeerSubsetSolver to choose replicas to request from.
        val peers             = connectionManager.connectedUsers.toArray
        val (aclOpGraph, acl) = syncInstance.currentAcl
        if peers.nonEmpty
        then
          if partialDeltaStore.isEmpty
          then
            val _ = connectionManager.sendMultiple(
              peers(rand.nextInt(peers.length)),
              RequestMissingRdt(rdtDeltas.allDots),
              RequestMissingAcl(aclOpGraph.heads, Set.empty)
            )
          else
            val partialDeltas = partialDeltaStore
            val requiredPerms =
              if partialDeltas.size < 100
              then // Only need to reconstruct what is missing
                partialDeltas.values.map(_.requiredPermissions).reduce(Lattice.merge)
              else // Request everything that is readable
                acl.read.getOrElse(localPublicId, PermissionTree.empty)

            // TODO: Could be optimized to reduce number of replicas to contact and deltas to request at the cost of complexity
            // - If a replica won't provide progress, don't request specific dot
            //    - progress can be used for this: !(goal ⋂ writer_permission <= progress)
            // - Provide existing progress to PartialReplicationPeerSubsetSolver
            // - Provide union of requiredPermissions of partial deltas instead of local read permission
            val (replicasToRequestFrom, _) = PartialReplicationPeerSubsetSolver.randomSubsetThatAllowsReconstruction(
              acl.write,
              requiredPerms
            )
            replicasToRequestFrom.foreach { remote =>
              val _ = connectionManager.sendMultiple(
                remote,
                RequestMissingRdt[RDT](rdtDeltas.allDots),
                RequestMissingAcl(aclOpGraph.heads, Set.empty)
              )
            }
      }
    }
  }
  antiEntropyThread.start()
}

object BftFilteringAntiEntropy {
  enum SyncMsg[RDT]:
    // TODO: Add bundle of (acl, rdt)-deltas as message type
    case RdtDelta(dot: Dot, delta: RDT, authorAclContext: Set[Signature], senderAclContext: Set[Signature])
    case AclDelta(encodedDelegation: EncodedDelegation)
    case AnnouncePeers(peers: Set[(PublicIdentity, (String, Int))])
    case RequestMissingRdt(rdtDots: Dots)
    // TODO: Improve anti entropy of ACL with bloom filter based sync
    case RequestMissingAcl(aclVersion: Set[Signature], knownMissing: Set[Signature])

  object SyncMsg {
    given codec[RDT](using JsonValueCodec[RDT]): JsonValueCodec[SyncMsg[RDT]] = {
      import lofi_acl.sync.JsoniterCodecs.uidKeyCodec
      JsonCodecMaker.make
    }
  }

}
