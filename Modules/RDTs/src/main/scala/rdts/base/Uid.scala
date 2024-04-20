package rdts.base

import java.util.Base64
import scala.util.Random

// opaque currently causes too many weird issues with library integrations, in particular the json libraries can no longer auto serialize
/* opaque */
type Uid = String

object Uid:
  given ordering: Ordering[Uid]  = Ordering.String
  def predefined(s: String): Uid = s
  def unwrap(id: Uid): String    = id
  def zero: Uid                  = ""
  private val random: Random     = scala.util.Random()

  extension (s: String) def asId: Uid = s

  /** Generates unique identifiers for use by CRDTs */
  def gen(): Uid =
    val randomBytes = new Array[Byte](6)
    random.nextBytes(randomBytes)
    Base64.getEncoder.encodeToString(randomBytes)
