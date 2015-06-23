package rescala.synchronization

import rescala.graph.{ParRPState, STMState}
import rescala.graph.{Reactive, State}
import rescala.turns.Engines.{Impl, synchron, unmanaged}
import rescala.turns.{Engine, Turn}

import scala.concurrent.stm.atomic

object Engines {

  def byName(name: String): Engine[_ <: State, _ <: Turn[_ <: State]] = name match {
    case "synchron" => synchron
    case "unmanaged" => unmanaged
    case "parrp" => parRP
    case "stm" => STM
    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }

  def all = List(STM, parRP, synchron, unmanaged)

  implicit val parRP: Engine[ParRPState.type, ParRP] = spinningWithBackoff(7)

  implicit val default: Engine[ParRPState.type, ParRP] = parRP

  implicit val STM: Engine[STMState.type, STMSync] = new Impl[STMState.type, STMSync](new STMSync()) {
    override def plan[R](i: Reactive[STMState.type]*)(f: STMSync => R): R = atomic { tx => super.plan(i: _*)(f) }
    override private[rescala] def bufferFactory: STMState.type = STMState
  }

  def spinningWithBackoff(backOff: Int): Impl[ParRPState.type, ParRP] = new Impl[ParRPState.type, ParRP](new ParRP(backOff)) {
    /** used for the creation of state inside reactives */
    override private[rescala] def bufferFactory: ParRPState.type = ParRPState
  }

}
