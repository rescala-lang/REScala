package lofi_acl.sync.acl.monotonic

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import lofi_acl.access.{Filter, Operation, PermissionTree}
import lofi_acl.collections.DeltaMapWithPrefix
import lofi_acl.crypto.{PrivateIdentity, PublicIdentity}
import lofi_acl.sync.*
import lofi_acl.sync.acl.monotonic.MonotonicAclSyncMessage.*
import lofi_acl.{access, sync}
import rdts.base.{Bottom, Lattice}
import rdts.time.{Dot, Dots}

import java.util.concurrent.atomic.AtomicReference

class SyncWithMonotonicAcl[RDT](
    private val localIdentity: PrivateIdentity,
    initialAclMessages: List[AclDelta[RDT]],
    initialRdt: DeltaMapWithPrefix[RDT], // Assumed to correspond with ACL!
)(using
    lattice: Lattice[RDT],
    bottom: Bottom[RDT],
    rdtJsonCode: JsonValueCodec[RDT],
    filter: Filter[RDT]
) extends Sync[RDT] {

  private val antiEntropy = FilteringAntiEntropy[RDT](localIdentity, initialAclMessages, initialRdt, this)
  @volatile private var antiEntropyThread: Option[Thread] = None

  private val localPublicId = localIdentity.getPublic

  private val rdtReference: AtomicReference[(Dots, RDT)] = AtomicReference(
    initialRdt.allDots ->
    initialRdt.prefix.merge(initialRdt.deltas.foldLeft(bottom.empty) { case (l, (_, r)) => l.merge(r) })
  )
  private val lastLocalRdtDot: AtomicReference[Dot] =
    AtomicReference(initialRdt._1.max(localPublicId.toUid).getOrElse(Dot(localPublicId.toUid, -1)))

  private val lastLocalAclDot: AtomicReference[Dot] = {
    val localId = localIdentity.getPublic.id
    AtomicReference(
      initialAclMessages
        .filter(_.dot.place.delegate == localId)
        .maxByOption(_.dot.time)
        .map(_.dot)
        .getOrElse(Dot(localIdentity.getPublic.toUid, -1))
    )
  }

  def grantPermissions(affectedUser: PublicIdentity, realm: PermissionTree, typeOfPermission: Operation): Unit = {
    val dot = lastLocalAclDot.updateAndGet(dot => dot.advance)
    antiEntropy.grantPermission(dot, affectedUser, realm, typeOfPermission)
  }

  def mutateRdt(deltaMutator: RDT => RDT): Unit = {
    val dot = lastLocalRdtDot.updateAndGet(dot => dot.advance)
    antiEntropy.mutateRdt(dot, deltaMutator(rdtReference.get()._2))
  }

  def connectionString: String = {
    s"${localPublicId.id}@localhost:${antiEntropy.listenPort}"
  }

  def connect(connectionString: String): Unit = {
    val parts = connectionString.split("@")
    require(parts.length == 2)
    val remoteUser = PublicIdentity(parts(0))
    val hostParts  = parts(1).split(":")
    require(hostParts.length == 2)
    antiEntropy.newPeers(Map(remoteUser -> (hostParts(0), hostParts(1).toInt)))
  }

  override def receivedDelta(dot: Dot, rdt: RDT): Unit =
    val _ = rdtReference.updateAndGet((dots, rdt) => dots.add(dot) -> rdt.merge(rdt))

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
