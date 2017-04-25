package rescala.stm

import rescala.engine._
import rescala.twoversion.TwoVersionEngineImpl

import scala.concurrent.stm.atomic

object STMEngine {
  implicit val stm: Engine[STMTurn, STMTurn] = new TwoVersionEngineImpl[STMTurn, STMTurn]("STM", new STMTurn()) {
    override private[rescala] def executeTurn[R](initialWrites: Traversable[Reactive], admissionPhase: STMTurn => R): R = atomic { tx => super.executeTurn(initialWrites, admissionPhase) }
  }
}
