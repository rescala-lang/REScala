package lofi_acl.sync.acl.monotonic

import lofi_acl.access.Operation
import lofi_acl.crypto.PublicIdentity
import rdts.time.{Dot, Dots}

enum MonotonicAclSyncMessage[RDT]:
  case AnnouncePeers(peers: Map[PublicIdentity, (String, Int)])
  case AddAclEntry(
      subject: PublicIdentity,
      path: String,
      operation: Operation,
      dot: Dot, // Also indicates authorship
      cc: Dots,
      signature: Array[Byte]
  )
  case Delta(delta: RDT, dot: Dot /* dot.place is the author */, aclCC: Dots)
  case RequestMissing(rdtDots: Dots, aclDots: Dots)
