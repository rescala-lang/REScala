package kofre.base

import java.util.{Base64, UUID}
import scala.util.Random

object Defs {
  def genTimestamp(): Long = System.currentTimeMillis

  opaque type Id = String

  type Time = Long

  object Id:
    given ordering: Ordering[Id] = Ordering.String

  val random: Random = scala.util.Random()

  def predefined(s: String): Id = s
  def zeroId: Id                = ""

  /** Generates unique identifiers for use by CRDTs */
  def genId(): Id =
    val randomBytes = new Array[Byte](15)
    random.nextBytes(randomBytes)
    Base64.getEncoder.encodeToString(randomBytes)
}
