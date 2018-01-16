package rescala.stm

import rescala.core._
import rescala.twoversion.TwoVersionSchedulerImpl

import scala.concurrent.stm.atomic

object STMEngine {
  implicit val stm: Scheduler[STMTurn] = new TwoVersionSchedulerImpl[STMTurn, STMTurn]("STM", () => new STMTurn()) {
    override protected[rescala] def executeTurn[R](initialWrites: Traversable[ReSource], admissionPhase: AdmissionTicket => R): R =
      atomic { tx => super.executeTurn(initialWrites, admissionPhase) }
  }
}
