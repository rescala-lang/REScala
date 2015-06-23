package rescala.synchronization

import rescala.graph._
import rescala.turns.Engines.{Impl, synchron, unmanaged}
import rescala.turns.{Engine, Turn}

import scala.concurrent.stm.atomic

object Engines {

  def byName(name: String): Engine[Turn] = name match {
    case "synchron" => synchron
    case "unmanaged" => unmanaged
    case "parrp" => parRP
    case "stm" => STM
    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }

  def all: List[Engine[Turn]] = List(STM, parRP, synchron, unmanaged)

  implicit val parRP: Engine[ParRP] = spinningWithBackoff(7)

  implicit val default: Engine[Turn] = parRP

  implicit val STM: Engine[STMSync] = new Impl(new STMSync()) {
    override def plan[R](i: Reactive*)(f: STMSync => R): R = atomic { tx => super.plan(i: _*)(f) }
    override private[rescala] def bufferFactory: SynchronizationFactory = JVMFactories.stm
  }

  def spinningWithBackoff(backOff: Int) = new Impl(new ParRP(backOff))

}
