package lofi_acl.sync.acl.monotonic

import lofi_acl.access.{Operation, PermissionTree}
import lofi_acl.crypto.PublicIdentity
import rdts.time.{Dot, Dots}

object MonotonicAclSyncMessage {
  case class Signature(sig: Array[Byte])
}

enum MonotonicAclSyncMessage[RDT]:
  case PermissionsInUse(minimumAclVersion: Dots, writePermission: PermissionTree)
  case AnnouncePeers(peers: Set[(PublicIdentity, (String, Int))])
  case AclDelta(
      subject: PublicIdentity,
      realm: PermissionTree,
      operation: Operation,
      dot: Dot, // Also indicates authorship
      cc: Dots,
      signature: MonotonicAclSyncMessage.Signature
  )
  case Delta(delta: RDT, dot: Dot /* dot.place is the author */, aclCC: Dots)
  case RequestMissing(rdtDots: Dots, aclDots: Dots)
