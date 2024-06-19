package lofi_acl.sync.no_acl

import lofi_acl.crypto.PublicIdentity
import rdts.time.Dots

enum MutuallyTrustingSyncMessage[RDT]:
  case AnnouncePeers(peers: Map[PublicIdentity, (String, Int)])
  case AddUsers(users: Set[PublicIdentity], dots: Dots, cc: Dots)
  case Delta(delta: RDT, dots: Dots, rdtCC: Dots, permCC: Dots)
  case Time(rdtTime: Dots, permTime: Dots)
  case RequestMissing(rdtMerged: Dots, rdtRx: Dots, permMerged: Dots, permRx: Dots)
