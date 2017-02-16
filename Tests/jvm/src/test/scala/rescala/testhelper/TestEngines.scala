package rescala.testhelper

import rescala.Engines
import rescala.engine.Engine
import rescala.pipelining.{PipelineEngine, PipelineStruct, PipeliningTurn}
import rescala.stm.STMTurn
import rescala.twoversion.EngineImpl

import scala.concurrent.stm.atomic

object TestEngines {

  implicit val pipeline: Engine[PipelineStruct.type, PipeliningTurn] = new PipelineEngine()
  implicit val stm: Engine[STMTurn, STMTurn] = new EngineImpl[STMTurn, STMTurn]("STM", new STMTurn()) {
    override def plan[R](i: Reactive*)(f: STMTurn => R): R = atomic { tx => super.plan(i: _*)(f) }
  }

  val all: List[Engines.TEngine] = stm :: /*pipeline ::*/ rescala.Engines.all
}
