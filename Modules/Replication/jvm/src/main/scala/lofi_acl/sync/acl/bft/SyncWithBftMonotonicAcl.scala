package lofi_acl.sync.acl.bft

import channels.tls.PrivateIdentity
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.PublicIdentity
import lofi_acl.access.{Filter, Operation, PermissionTree}
import lofi_acl.sync.*
import lofi_acl.sync.acl.Sync
import lofi_acl.sync.acl.bft.BftAclOpGraph.{Delegation, EncodedDelegation, Signature}
import lofi_acl.{access, sync}
import rdts.base.{Bottom, Lattice, Uid}
import rdts.time.{Dot, Dots}

import java.util.concurrent.atomic.AtomicReference
import scala.util.{Failure, Success}

class SyncWithBftMonotonicAcl[RDT](
    private val localIdentity: PrivateIdentity,
    aclRoot: EncodedDelegation,
    onDeltaReceive: RDT => Unit = (_: RDT) => {} // Consumes a delta
)(using
    lattice: Lattice[RDT],
    bottom: Bottom[RDT],
    rdtJsonCodec: JsonValueCodec[RDT],
    filter: Filter[RDT]
) extends Sync[RDT] {

  private val antiEntropy                                 = BftFilteringAntiEntropy[RDT](localIdentity, aclRoot, this)
  @volatile private var antiEntropyThread: Option[Thread] = None

  private val localPublicId = localIdentity.getPublic

  private val rdtReference: AtomicReference[(Dots, RDT)] = AtomicReference(Dots.empty -> Bottom[RDT].empty)
  private val lastLocalRdtDot: AtomicReference[Dot]      = AtomicReference(Dot(Uid(localPublicId.id), -1))

  def state: RDT                       = rdtReference.get()._2
  def currentAcl: (BftAclOpGraph, Acl) = localAcl.get()
  // Only change using grantPermissions!
  private val localAcl: AtomicReference[(BftAclOpGraph, Acl)] = {
    aclRoot.decode match
      case Failure(exception) => throw exception
      case Success((sig, delegation)) =>
        val opGraph = BftAclOpGraph(Map(sig -> delegation), Set(sig))
        AtomicReference((opGraph, opGraph.reconstruct(Set(sig)).get))
  }

  def grantPermissions(affectedUser: PublicIdentity, realm: PermissionTree, typeOfPermission: Operation): Unit = {
    val (read, write) = typeOfPermission match
      case lofi_acl.access.Operation.READ  => (realm, PermissionTree.empty)
      case lofi_acl.access.Operation.WRITE => (realm, realm)

    localAcl.synchronized {
      val old @ (opGraph, acl) = localAcl.get()
      val privateKey           = localIdentity.identityKey.getPrivate

      val (updatedOpGraph, aclDelta) = opGraph.delegateAccess(localPublicId, privateKey, affectedUser, read, write)
      val updatedAcl                 = acl.addPermissions(affectedUser, read, write)
      require(localAcl.compareAndSet(old, (updatedOpGraph, updatedAcl)))

      antiEntropy.broadcastAclDelegation(aclDelta)
    }
  }

  def applyAclIfPossible(encodedDelegation: EncodedDelegation): Set[Signature] = {
    localAcl.synchronized {
      val old @ (opGraph, acl) = localAcl.get()
      opGraph.receive(encodedDelegation.sig, encodedDelegation.op) match
        case Left(missingSignatures) => return missingSignatures
        case Right(updatedOpGraph) =>
          val updatedAcl = updatedOpGraph.reconstruct(updatedOpGraph.heads).get
          assert {
            val delegation = encodedDelegation.decode.get._2
            updatedAcl == acl.addPermissions(delegation.delegatee, delegation.read, delegation.write)
          }
          require(localAcl.compareAndSet(old, (updatedOpGraph, updatedAcl)))
          Set.empty
    }
  }

  def mutateRdt(deltaMutator: RDT => RDT): Unit = {
    val dot = lastLocalRdtDot.updateAndGet(dot => dot.advance)
    antiEntropy.mutateRdt(dot, deltaMutator(rdtReference.get()._2))
  }

  def connectionString: String = {
    s"localhost:${antiEntropy.listenPort.getOrElse(-1)}"
  }

  def connect(remoteUser: PublicIdentity, remoteAddress: String): Unit = {
    val hostParts = remoteAddress.split(":")
    require(hostParts.length == 2)
    antiEntropy.newPeers(Set(remoteUser -> (hostParts(0), hostParts(1).toInt)))
  }

  override def receivedDelta(dot: Dot, delta: RDT): Unit =
    val _ = rdtReference.updateAndGet((dots, rdt) => dots.add(dot) -> rdt.merge(delta))
    onDeltaReceive(delta)

  def start(): Unit = {
    synchronized {
      require(antiEntropyThread.isEmpty)
      antiEntropyThread = Some(antiEntropy.start())
    }
  }

  def stop(): Unit = {
    synchronized {
      require(antiEntropyThread.nonEmpty)
      antiEntropy.stop()
      antiEntropyThread.get.interrupt()
      antiEntropyThread = None
    }
  }
}

object SyncWithBftMonotonicAcl {
  def createAsRootOfTrust[RDT](rootIdentity: PrivateIdentity)(using
      lattice: Lattice[RDT],
      bottom: Bottom[RDT],
      rdtJsonCodec: JsonValueCodec[RDT],
      filter: Filter[RDT],
  ): SyncWithBftMonotonicAcl[RDT] = {
    val aclRoot = BftAclOpGraph.createSelfSignedRoot(rootIdentity)
    SyncWithBftMonotonicAcl(rootIdentity, aclRoot)
  }
}
