package rescala.engines

import rescala.engines.Engines.{synchron, synchronFair, unmanaged}
import rescala.graph.Struct
import rescala.parrp.{Backoff, ParRP, ParRPStruct}
import rescala.pipelining.{PipelineEngine, PipelineStruct, PipeliningTurn}
import rescala.propagation.Turn
import rescala.stm.{STMTurn, STMStruct}

import scala.concurrent.stm.atomic
import scala.language.existentials

object JVMEngines {

  type TEngine = Engine[S, Turn[S]] forSome { type S <: Struct }

  def byName[S <: Struct](name: String): Engine[S, Turn[S]] = name match {
    case "synchron" => synchron.asInstanceOf[Engine[S, Turn[S]]]
    case "unmanaged" => unmanaged.asInstanceOf[Engine[S, Turn[S]]]
    case "parrp" => parrp.asInstanceOf[Engine[S, Turn[S]]]
    case "stm" => stm.asInstanceOf[Engine[S, Turn[S]]]
    case "fair" => synchronFair.asInstanceOf[Engine[S, Turn[S]]]
    case "pipeline" => pipeline.asInstanceOf[Engine[S, Turn[S]]]

    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }

  def all: List[TEngine] = List[TEngine](stm, parrp, synchron, unmanaged, synchronFair)

  implicit val parrp: Engine[ParRPStruct.type, ParRP] = spinningWithBackoff(() => new Backoff)

  implicit val default: Engine[ParRPStruct.type, ParRP] = parrp

  implicit val pipeline: Engine[PipelineStruct.type, PipeliningTurn] = new PipelineEngine()

  implicit val stm: Engine[STMStruct.type, STMTurn] = new EngineImpl[STMStruct.type, STMTurn](STMStruct, new STMTurn()) {
    override def plan[R](i: Reactive*)(f: STMTurn => R): R = atomic { tx => super.plan(i: _*)(f) }
  }

  def spinningWithBackoff(backOff: () => Backoff): Engine[ParRPStruct.type, ParRP] = new EngineImpl[ParRPStruct.type, ParRP](ParRPStruct, new ParRP(backOff()))

}
