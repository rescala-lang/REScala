package rescala.engines

import java.util.concurrent.{Executor, Executors}

import rescala.graph.Struct
import rescala.parrp._
import rescala.propagation.Turn
import rescala.stm.STMTurn

import scala.concurrent.stm.atomic
import scala.language.existentials

object Engines extends CommonEngines {


  def byName[S <: Struct](name: String): Engine[S, Turn[S]] = name match {
    case "synchron" => synchron.asInstanceOf[Engine[S, Turn[S]]]
    case "unmanaged" => unmanaged.asInstanceOf[Engine[S, Turn[S]]]
    case "parrp" => parrp.asInstanceOf[Engine[S, Turn[S]]]
    case "stm" => stm.asInstanceOf[Engine[S, Turn[S]]]
    case "fair" => synchronFair.asInstanceOf[Engine[S, Turn[S]]]
    case "locksweep" => locksweep.asInstanceOf[Engine[S, Turn[S]]]

    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }

  def all: List[TEngine] = List[TEngine](unmanaged, parrp, stm, locksweep)

  implicit val parrp: Engine[ParRP, ParRP] = parrpWithBackoff(() => new Backoff)

  implicit val locksweep: Engine[LSStruct.type, LockSweep] = locksweepWithBackoff(() => new Backoff())

  implicit val parallellocksweep: EngineImpl[LSStruct.type, ParallelLockSweep] = {
    val ex: Executor = Executors.newWorkStealingPool()
    new EngineImpl[LSStruct.type, ParallelLockSweep]("ParallelLockSweep", engine => new ParallelLockSweep(new Backoff(), ex, engine))
  }

  implicit val default: Engine[ParRP, ParRP] = parrp

  implicit val stm: Engine[STMTurn, STMTurn] = new EngineImpl[STMTurn, STMTurn]("STM", new STMTurn()) {
    override def plan[R](i: Reactive*)(f: STMTurn => R): R = atomic { tx => super.plan(i: _*)(f) }
  }

  def locksweepWithBackoff(backOff: () => Backoff):Engine[LSStruct.type, LockSweep]  = new EngineImpl[LSStruct.type, LockSweep]("LockSweep", new LockSweep(backOff()))
  def parrpWithBackoff(backOff: () => Backoff): Engine[ParRP, ParRP] = new EngineImpl[ParRP, ParRP]("ParRP", new ParRP(backOff()))

}
