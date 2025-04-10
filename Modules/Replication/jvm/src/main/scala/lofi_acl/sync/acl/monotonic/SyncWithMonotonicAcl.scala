package lofi_acl.sync.acl.monotonic

import channels.tls.PrivateIdentity
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import crypto.PublicIdentity
import lofi_acl.access.{Filter, Operation, PermissionTree}
import lofi_acl.collections.DeltaMapWithPrefix
import lofi_acl.sync.*
import lofi_acl.sync.JsoniterCodecs.messageJsonCodec
import lofi_acl.sync.acl.Sync
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.*
import lofi_acl.{access, sync}
import rdts.base.{Bottom, Lattice, Uid}
import rdts.time.{Dot, Dots}

import java.util.concurrent.atomic.AtomicReference

class SyncWithMonotonicAcl[RDT](
    private val localIdentity: PrivateIdentity,
    rootOfTrust: PublicIdentity,
    initialAclDeltas: List[AclDelta[RDT]],
    initialRdt: DeltaMapWithPrefix[RDT],         // Assumed to correspond with ACL!
    onDeltaReceive: RDT => Unit = (_: RDT) => {} // Consumes a delta
)(using
    lattice: Lattice[RDT],
    bottom: Bottom[RDT],
    rdtJsonCode: JsonValueCodec[RDT],
    filter: Filter[RDT]
) extends Sync[RDT] {

  private val antiEntropy = FilteringAntiEntropy[RDT](localIdentity, rootOfTrust, initialAclDeltas, initialRdt, this)
  @volatile private var antiEntropyThread: Option[Thread] = None

  private val localPublicId = localIdentity.getPublic

  def state: RDT = rdtReference.get()._2
  private val rdtReference: AtomicReference[(Dots, RDT)] = AtomicReference(
    initialRdt.allDots ->
    initialRdt.prefix.merge(initialRdt.deltas.foldLeft(bottom.empty) { case (l, (_, r)) => l.merge(r) })
  )
  private val lastLocalRdtDot: AtomicReference[Dot] =
    AtomicReference(initialRdt._1.max(Uid(localPublicId.id)).getOrElse(Dot(Uid(localPublicId.id), -1)))

  private val lastLocalAclDot: AtomicReference[Dot] = {
    val localId = localIdentity.getPublic.id
    AtomicReference(
      initialAclDeltas
        .filter(_.dot.place.delegate == localId)
        .maxByOption(_.dot.time)
        .map(_.dot)
        .getOrElse(Dot(Uid(localIdentity.getPublic.id), -1))
    )
  }

  def currentAcl: MonotonicAcl[RDT] = antiEntropy.acl

  def grantPermissions(affectedUser: PublicIdentity, realm: PermissionTree, typeOfPermission: Operation): Unit = {
    val dot = lastLocalAclDot.updateAndGet(dot => dot.advance)
    antiEntropy.grantPermission(dot, affectedUser, realm, typeOfPermission)
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

object SyncWithMonotonicAcl {
  def createAsRootOfTrust[RDT](rootIdentity: PrivateIdentity)(using
      lattice: Lattice[RDT],
      bottom: Bottom[RDT],
      rdtJsonCode: JsonValueCodec[RDT],
      filter: Filter[RDT],
      msgJsonCodec: JsonValueCodec[MonotonicAclSyncMessage[RDT]]
  ): SyncWithMonotonicAcl[RDT] = {
    val selfSignedAclDelta: AclDelta[RDT] = MonotonicAcl.createRootOfTrust[RDT](rootIdentity)
    SyncWithMonotonicAcl(rootIdentity, rootIdentity.getPublic, List(selfSignedAclDelta), DeltaMapWithPrefix.empty)
  }
}
