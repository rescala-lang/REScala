package rdts.base

import java.nio.ByteBuffer
import java.util.Base64
import scala.CanEqual
import scala.annotation.implicitNotFound

// opaque currently causes too many weird issues with library integrations, in particular the json libraries can no longer auto serialize
/** Uid’s are serializable abstract unique Ids. Currently implemented as Strings, but subject to change. */
case class Uid(delegate: String) derives CanEqual {
  override def toString: String = show
  def show: String              = s"🪪$delegate"
}

object Uid:
  given ordering: Ordering[Uid]  = Ordering.String.on(_.delegate)
  def predefined(s: String): Uid = Uid(s)
  def unwrap(id: Uid): String    = id.delegate
  val zero: Uid                  = Uid("")

  extension (s: String) def asId: Uid = Uid(s)

  private var idCounter: Long   = scala.util.Random.nextLong()
  private val bytes: ByteBuffer = ByteBuffer.wrap(new Array[Byte](8))

  /** Generate a new unique ID.
    * Note, currently collisions are possible as we only ues 6 bytes of random values.
    * Why 6? Because multiples of 3 nicely base64 encode
    */
  def gen(): Uid = synchronized {
    idCounter = (idCounter + 1) % (1L << 48)
    bytes.rewind()
    val idBytes2 = bytes.putLong(idCounter).array().drop(2)
    Uid(Base64.getEncoder.encodeToString(idBytes2))
  }

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
