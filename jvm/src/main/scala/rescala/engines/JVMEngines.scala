package rescala.engines

import rescala.engines.Engines.{synchron, synchronFair, unmanaged}
import rescala.graph.Spores
import rescala.parrp.{Backoff, ParRP, ParRPSpores}
import rescala.pipelining.{PipelineEngine, PipelineSpores, PipeliningTurn}
import rescala.propagation.Turn
import rescala.stm.{STMTurn, STMSpores}

import scala.concurrent.stm.atomic
import scala.language.existentials

object JVMEngines {

  type TEngine = Engine[S, Turn[S]] forSome { type S <: Spores }

  def byName[S <: Spores](name: String): Engine[S, Turn[S]] = name match {
    case "synchron" => synchron.asInstanceOf[Engine[S, Turn[S]]]
    case "unmanaged" => unmanaged.asInstanceOf[Engine[S, Turn[S]]]
    case "parrp" => parrp.asInstanceOf[Engine[S, Turn[S]]]
    case "stm" => stm.asInstanceOf[Engine[S, Turn[S]]]
    case "fair" => synchronFair.asInstanceOf[Engine[S, Turn[S]]]
    case "pipeline" => pipeline.asInstanceOf[Engine[S, Turn[S]]]

    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }

  def all: List[TEngine] = List[TEngine](stm, parrp, synchron, unmanaged, synchronFair)

  implicit val parrp: Engine[ParRPSpores.type, ParRP] = spinningWithBackoff(() => new Backoff)

  implicit val default: Engine[ParRPSpores.type, ParRP] = parrp

  implicit val pipeline: Engine[PipelineSpores.type, PipeliningTurn] = new PipelineEngine()

  implicit val stm: Engine[STMSpores.type, STMTurn] = new EngineImpl[STMSpores.type, STMTurn](STMSpores, new STMTurn()) {
    override def plan[R](i: Reactive*)(f: STMTurn => R): R = atomic { tx => super.plan(i: _*)(f) }
  }

  def spinningWithBackoff(backOff: () => Backoff): Engine[ParRPSpores.type, ParRP] = new EngineImpl[ParRPSpores.type, ParRP](ParRPSpores, new ParRP(backOff()))

}
