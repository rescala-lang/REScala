package kofre

import java.util.{Base64, UUID}

object IdUtil {
  def genTimestamp(): Long = System.currentTimeMillis

  type Id = String

  type Time = Long

  object Id:
    given ordering: Ordering[Id] = summon[Ordering[String]]

  val random = scala.util.Random()

  def predefined(s: String): Id = s
  def zero: Id                  = ""

  /** Generates unique identifiers for use by CRDTs */
  def genId(): Id =
    val randomBytes = new Array[Byte](15)
    random.nextBytes(randomBytes)
    Base64.getEncoder.encodeToString(randomBytes)
}
