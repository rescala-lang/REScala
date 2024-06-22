package rdts.syntax

import rdts.base.Uid

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Requires a replica ID of the current local replica that is doing the modification."
)
/** Operations may require an ID of the replica doing a modification.
  * We provide it as it’s own opaque type, to make it obvious that this should not be just any ID.
  * Use [[Uid]] if you want to store an ID in a replicated data structure.
  */
case class LocalUid(uid: Uid) {
  override def toString: String = show
  def show: String              = uid.show
}
object LocalUid:
  given ordering: Ordering[LocalUid] = Uid.ordering.on(_.uid)
  inline given fromId: Conversion[Uid, LocalUid] with {
    override def apply(x: Uid): LocalUid = LocalUid(x)
  }
  def predefined(s: String): LocalUid     = LocalUid.fromId(Uid.predefined(s))
  def unwrap(id: LocalUid): Uid           = id.uid
  def gen(): LocalUid                     = Uid.gen()
  def replicaId(using rid: LocalUid): Uid = rid.uid
