package rescala.stm

import rescala.core._
import rescala.twoversion.TwoVersionSchedulerImpl

import scala.concurrent.stm.atomic

object STMEngine {
  implicit val stm: Scheduler[STMTurn] = new TwoVersionSchedulerImpl[STMTurn, STMTurn]("STM", () => new STMTurn()) {
    override def executeTurn[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R =
      atomic { tx => super.executeTurn(initialWrites, admissionPhase) }
  }
}
