package rescala.extra.lattices

import java.util.UUID


object IdUtil {
  def genTimestamp(): Long = System.currentTimeMillis

  type Id = String
  /** Generates unique identifiers for use by CRDTs */
  def genId(): Id = UUID.randomUUID().toString
}
