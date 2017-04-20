package rescala.testhelper

import rescala.Engines
import rescala.engine.Engine
import rescala.stm.STMTurn
import rescala.twoversion.EngineImpl

import scala.concurrent.stm.atomic

object TestEngines {

  implicit val stm: Engine[STMTurn, STMTurn] = new EngineImpl[STMTurn, STMTurn]("STM", new STMTurn()) {
    override def transaction[R](i: Reactive*)(f: STMTurn => R): R = atomic { tx => super.transaction(i: _*)(f) }
  }

  val all: List[Engines.TEngine] = rescala.Engines.all ::: stm :: Nil
}
