package rescala.stm

import rescala.engine._
import rescala.twoversion.TwoVersionEngineImpl

import scala.concurrent.stm.atomic

object STMEngine {
  implicit val stm: Engine[STMTurn, STMTurn] = new TwoVersionEngineImpl[STMTurn, STMTurn]("STM", new STMTurn()) {
    override def transaction[R](i: Reactive*)(f: STMTurn => R): R = atomic { tx => super.transaction(i: _*)(f) }
  }
}
