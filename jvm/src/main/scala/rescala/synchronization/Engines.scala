package rescala.synchronization

import rescala.graph.{ParRPSpores, Reactive, STMSpores, Spores}
import rescala.turns.Engines.{Impl, synchron, unmanaged, synchronFair}
import rescala.turns.{Engine, Turn}

import scala.concurrent.stm.atomic
import scala.language.existentials

object Engines {

  type TEngine = Engine[S, Turn[S]] forSome { type S <: Spores }

  def byName[S <: Spores](name: String): Engine[S, Turn[S]] = name match {
    case "synchron" => synchron.asInstanceOf[Engine[S, Turn[S]]]
    case "unmanaged" => unmanaged.asInstanceOf[Engine[S, Turn[S]]]
    case "parrp" => parrp.asInstanceOf[Engine[S, Turn[S]]]
    case "stm" => stm.asInstanceOf[Engine[S, Turn[S]]]
    case "fair" => synchronFair.asInstanceOf[Engine[S, Turn[S]]]

    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }

  def all: List[TEngine] = List[TEngine](stm, parrp, synchron, unmanaged, synchronFair)

  implicit val parrp: Engine[ParRPSpores.type, ParRP] = spinningWithBackoff(() => new Backoff)

  implicit val default: Engine[ParRPSpores.type, ParRP] = parrp

  implicit val stm: Engine[STMSpores.type, STMSync] = new Impl[STMSpores.type, STMSync](STMSpores, new STMSync()) {
    override def plan[R](i: Reactive*)(f: STMSync => R): R = atomic { tx => super.plan(i: _*)(f) }
  }

  def spinningWithBackoff(backOff: () => Backoff): Impl[ParRPSpores.type, ParRP] = new Impl[ParRPSpores.type, ParRP](ParRPSpores, new ParRP(backOff()))

}
