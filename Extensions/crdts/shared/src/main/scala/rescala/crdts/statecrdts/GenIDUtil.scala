package rescala.crdts.statecrdts

object GenIDUtil {
  /** Generates unique identifiers for use by CRDTs */
  def genId: String = java.util.UUID.randomUUID().toString
}
