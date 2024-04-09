package rescala

import rescala.core.{AdmissionTicket, ReevTicket, StaticTicket}
import rescala.default.*
import rescala.scheduler.Twoversion

package object Lul extends Twoversion {
  def dynamicTicket(implicit at: AdmissionTicket[BundleState]): StaticTicket[BundleState] =
    at.tx.asInstanceOf[TwoVersionTransactionImpl].makeDynamicReevaluationTicket[Boolean, Null](false).asInstanceOf[StaticTicket[BundleState]]
}

