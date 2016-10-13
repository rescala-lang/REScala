import rescala.graph.Struct
import rescala.propagation.Turn
import rescala.engines._

import rescala.stm.STMTurn

import scala.concurrent.stm.atomic

object STMEngine {
  implicit val stm: Engine[STMTurn, STMTurn] = new EngineImpl[STMTurn, STMTurn]("STM", new STMTurn()) {
    override def plan[R](i: Reactive*)(f: STMTurn => R): R = atomic { tx => super.plan(i: _*)(f) }
  }
}
