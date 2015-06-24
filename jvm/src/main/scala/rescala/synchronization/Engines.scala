package rescala.synchronization

import rescala.graph.{ParRPState, Reactive, STMState, State}
import rescala.turns.Engines.{Impl, synchron, unmanaged}
import rescala.turns.{Engine, Turn}

import scala.concurrent.stm.atomic
import scala.language.existentials

object Engines {

  type TEngine = Engine[S, Turn[S]] forSome { type S <: State }

  def byName[S <: State](name: String): Engine[S, Turn[S]] = name match {
    case "synchron" => synchron.asInstanceOf[Engine[S, Turn[S]]]
    case "unmanaged" => unmanaged.asInstanceOf[Engine[S, Turn[S]]]
    case "parrp" => parRP.asInstanceOf[Engine[S, Turn[S]]]
    case "stm" => STM.asInstanceOf[Engine[S, Turn[S]]]
    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }

  def all: List[TEngine] = List[TEngine](STM, parRP, synchron, unmanaged)

  implicit val parRP: Engine[ParRPState.type, ParRP] = spinningWithBackoff(7)

  implicit val default: Engine[ParRPState.type, ParRP] = parRP

  implicit val STM: Engine[STMState.type, STMSync] = new Impl[STMState.type, STMSync](STMState, new STMSync()) {
    override def plan[R](i: Reactive[STMState.type]*)(f: STMSync => R): R = atomic { tx => super.plan(i: _*)(f) }
  }

  def spinningWithBackoff(backOff: Int): Impl[ParRPState.type, ParRP] = new Impl[ParRPState.type, ParRP](ParRPState, new ParRP(backOff))

}
