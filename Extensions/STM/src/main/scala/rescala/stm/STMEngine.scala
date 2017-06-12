package rescala.stm

import rescala.engine._
import rescala.twoversion.TwoVersionEngineImpl

import scala.concurrent.stm.atomic

object STMEngine {
  implicit val stm: Engine[STMTurn] = new TwoVersionEngineImpl[STMTurn, STMTurn]("STM", new STMTurn()) {
    override protected[rescala] def executeTurn[I, R](initialWrites: Traversable[Reactive], admissionPhase: AdmissionTicket => I, wrapUpPhase: (I, WrapUpTicket) => R): R = atomic { tx => super.executeTurn(initialWrites, admissionPhase, wrapUpPhase) }
  }
}
