package lofi_acl.sync.acl.monotonic

import rdts.time.Dot

trait Sync[RDT] {
  def receivedDelta(dot: Dot, rdt: RDT): Unit
  def connect(connectionString: String): Unit
  def connectionString: String
}
