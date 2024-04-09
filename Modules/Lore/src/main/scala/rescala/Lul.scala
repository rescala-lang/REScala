package reactives

import reactives.core.{AdmissionTicket, ReevTicket, StaticTicket}
import reactives.default.*
import reactives.scheduler.Twoversion
import reactives.operator.Interface.State as BundleState


package object Lul extends Twoversion {
  def dynamicTicket(implicit at: AdmissionTicket[BundleState]): StaticTicket[BundleState] =
    at.tx.asInstanceOf[TwoVersionTransactionImpl].makeDynamicReevaluationTicket[Boolean, Null](false).asInstanceOf[StaticTicket[BundleState]]
}

