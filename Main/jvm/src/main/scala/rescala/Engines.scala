package rescala

import java.util.concurrent.{Executor, Executors}

import rescala.engine._
import rescala.graph.Struct
import rescala.levelbased.LevelBasedPropagationEngines
import rescala.parrp._
import rescala.twoversion.EngineImpl

import scala.language.existentials

object Engines extends LevelBasedPropagationEngines {

  def byName[S <: Struct](name: String): Engine[S, propagation.Turn[S]] = name match {
    case "synchron" => synchron.asInstanceOf[Engine[S, propagation.Turn[S]]]
    case "unmanaged" => unmanaged.asInstanceOf[Engine[S, propagation.Turn[S]]]
    case "parrp" => parrp.asInstanceOf[Engine[S, propagation.Turn[S]]]
    case "locksweep" => locksweep.asInstanceOf[Engine[S, propagation.Turn[S]]]

    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }

  def all: List[TEngine] = List[TEngine](unmanaged, parrp, locksweep)

  implicit val parrp: Engine[ParRP, ParRP] = parrpWithBackoff(() => new Backoff)

  implicit val locksweep: Engine[LSStruct, LockSweep] = locksweepWithBackoff(() => new Backoff())

  implicit val parallellocksweep: EngineImpl[LSStruct, ParallelLockSweep] = {
    val ex: Executor = Executors.newWorkStealingPool()
    new EngineImpl[LSStruct, ParallelLockSweep]("ParallelLockSweep", (engine, prior) => new ParallelLockSweep(new Backoff(), ex, engine, prior))
  }

  implicit val default: Engine[ParRP, ParRP] = parrp

  def locksweepWithBackoff(backOff: () => Backoff): Engine[LSStruct, LockSweep]  = new EngineImpl[LSStruct, LockSweep]("LockSweep", (_, prior) => new LockSweep(backOff(), prior))
  def parrpWithBackoff(backOff: () => Backoff): Engine[ParRP, ParRP] = new EngineImpl[ParRP, ParRP]("ParRP", (_, prior) => new ParRP(backOff(), prior))

}
