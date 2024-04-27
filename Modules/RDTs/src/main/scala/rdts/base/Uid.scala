package rdts.base

import java.util.Base64
import scala.util.Random

// opaque currently causes too many weird issues with library integrations, in particular the json libraries can no longer auto serialize
case class Uid(delegate: String) {
  override def toString: String = show
  def show: String = s"🪪$delegate"
}

object Uid:
  given ordering: Ordering[Uid]  = Ordering.String.on(_.delegate)
  def predefined(s: String): Uid = Uid(s)
  def unwrap(id: Uid): String    = id.delegate
  val zero: Uid                  = Uid("")
  private val random: Random     = scala.util.Random()

  extension (s: String) def asId: Uid = Uid(s)

  /** Generates unique identifiers for use by CRDTs */
  def gen(): Uid =
    val randomBytes = new Array[Byte](6)
    random.nextBytes(randomBytes)
    Uid(Base64.getEncoder.encodeToString(randomBytes))
