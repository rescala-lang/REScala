package kofre.base

import java.util.{Base64, UUID}
import scala.util.Random

opaque type Uid = String

object Uid:
  given ordering: Ordering[Uid]  = Ordering.String
  def predefined(s: String): Uid = s
  def unwrap(id: Uid): String    = id
  def zero: Uid                  = ""
  val random: Random            = scala.util.Random()

  extension (s: String) def asId: Uid = s

  /** Generates unique identifiers for use by CRDTs */
  def gen(): Uid =
    val randomBytes = new Array[Byte](6)
    random.nextBytes(randomBytes)
    Base64.getEncoder.encodeToString(randomBytes)
