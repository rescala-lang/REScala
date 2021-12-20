package kofre

import java.security.SecureRandom
import java.util.{Base64, UUID}

object IdUtil {
  def genTimestamp(): Long = System.currentTimeMillis

  type Id = String

  val random: SecureRandom = SecureRandom()

  /** Generates unique identifiers for use by CRDTs */
  def genId(): Id =
    val randomBytes = new Array[Byte](15)
    random.nextBytes(randomBytes)
    Base64.getEncoder.encodeToString(randomBytes)
}
