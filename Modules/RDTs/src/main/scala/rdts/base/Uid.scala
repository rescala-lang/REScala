package rdts.base

import scala.CanEqual
import scala.annotation.implicitNotFound

// opaque currently causes too many weird issues with library integrations, in particular the json libraries can no longer auto serialize
/** Uid’s are serializable abstract unique Ids. Currently implemented as Strings, but subject to change. */
case class Uid(delegate: String) derives CanEqual {
  override def toString: String = show
  def show: String =
    val offset    = delegate.indexOf('.')
    val shortened = if offset > 0 then delegate.substring(0, offset + 4) else delegate
    s"🪪$shortened"
}

object Uid:
  given ordering: Ordering[Uid]  = Ordering.String.on(_.delegate)
  def predefined(s: String): Uid = Uid(s)
  def unwrap(id: Uid): String    = id.delegate
  val zero: Uid                  = Uid("")

  extension (s: String) def asId: Uid = Uid(s)

  inline given toLocal: Conversion[Uid, LocalUid] with {
    override def apply(x: Uid): LocalUid = LocalUid(x)
  }

  val jvmID: String = Base64.encode(scala.util.Random.nextLong(1L << 48))

  private var idCounter: Long = -1

  /** Generate a new unique ID.
    * Uses 48 bit of a process local random value + up to 64 of a counter.
    * Encoded as a string using 9 bytes + 1 byte per 6 bits of the counter value.
    */
  def gen(): Uid = synchronized {
    idCounter = (idCounter + 1)

    if idCounter != 0 then Uid(s"${Base64.encode(idCounter)}.$jvmID")
    else Uid(s"$jvmID")
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
object LocalUid {
  given ordering: Ordering[LocalUid] = Uid.ordering.on(_.uid)

  extension (s: String) def asId: LocalUid = predefined(s)

  def predefined(s: String): LocalUid     = Uid.predefined(s).convert
  def unwrap(id: LocalUid): Uid           = id.uid
  def gen(): LocalUid                     = Uid.gen().convert
  def replicaId(using rid: LocalUid): Uid = rid.uid
}

object Base64 {
  private val alphabet: Array[Char] = Array(
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
    'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', '_'
  )

  private val sb = StringBuilder(12)

  /** Encodes the 6 least significant bits to the start of the string.
    * It is thus very unlikely to match any other base64 encodings of longs.
    */
  def encode(long: Long): String = synchronized {
    sb.clear()
    var remaining = long
    while remaining != 0 do
      sb.append(alphabet((remaining & 63).toInt))
      remaining = remaining >>> 6
    sb.result()
  }
}
