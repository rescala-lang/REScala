package rescala.crdts.statecrdts

import java.util.UUID

object IdUtil {
  type Id = UUID
  /** Generates unique identifiers for use by CRDTs */
  def genId: Id = UUID.randomUUID()
}
