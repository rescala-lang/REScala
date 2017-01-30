package rescala.testhelper

import rescala.engines.{Engine, EngineImpl}
import rescala.pipelining.{PipelineEngine, PipelineStruct, PipeliningTurn}
import rescala.stm.STMTurn

import scala.concurrent.stm.atomic

object TestEngines {

  implicit val pipeline: Engine[PipelineStruct.type, PipeliningTurn] = new PipelineEngine()
  implicit val stm: Engine[STMTurn, STMTurn] = new EngineImpl[STMTurn, STMTurn]("STM", new STMTurn()) {
    override def plan[R](i: Reactive*)(f: STMTurn => R): R = atomic { tx => super.plan(i: _*)(f) }
  }

  val all: List[rescala.engines.Engines.TEngine] = stm :: /*pipeline ::*/ rescala.engines.Engines.all
}
