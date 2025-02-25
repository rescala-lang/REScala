package lofi_acl.sync.acl

import lofi_acl.crypto.PublicIdentity
import rdts.time.Dot

trait Sync[RDT] {
  def receivedDelta(dot: Dot, rdt: RDT): Unit
  def connect(remoteUser: PublicIdentity, connectionString: String): Unit
  def connectionString: String
}
