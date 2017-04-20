package rescala.stm

import rescala.engine._
import rescala.twoversion.EngineImpl

import scala.concurrent.stm.atomic

object STMEngine {
  implicit val stm: Engine[STMTurn, STMTurn] = new EngineImpl[STMTurn, STMTurn]("STM", new STMTurn()) {
    override def transaction[R](i: Reactive*)(f: STMTurn => R): R = atomic { tx => super.transaction(i: _*)(f) }
  }
}
