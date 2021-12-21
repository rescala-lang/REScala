package kofre

import java.util.{Base64, UUID}

object IdUtil {
  def genTimestamp(): Long = System.currentTimeMillis

  type Id = String

  val random = scala.util.Random()

  /** Generates unique identifiers for use by CRDTs */
  def genId(): Id =
    val randomBytes = new Array[Byte](15)
    random.nextBytes(randomBytes)
    val res = Base64.getEncoder.encodeToString(randomBytes)
    println(res)
    res
}
