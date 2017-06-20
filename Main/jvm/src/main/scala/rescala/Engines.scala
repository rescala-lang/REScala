package rescala

import java.util.concurrent.{Executor, Executors}

import rescala.core.{Engine, Struct}
import rescala.levelbased.LevelBasedPropagationEngines
import rescala.parrp._
import rescala.twoversion.TwoVersionEngineImpl

object Engines extends LevelBasedPropagationEngines {

  def byName[S <: Struct](name: String): Engine[S] = name match {
    case "synchron" => synchron.asInstanceOf[Engine[S]]
    case "unmanaged" => unmanaged.asInstanceOf[Engine[S]]
    case "parrp" => parrp.asInstanceOf[Engine[S]]
    case "locksweep" => locksweep.asInstanceOf[Engine[S]]

    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }
  def all: List[Engine[_ <: Struct]] = List(unmanaged, parrp, locksweep)

  implicit val parrp: Engine[ParRP] = parrpWithBackoff(() => new Backoff)

  implicit val locksweep: Engine[LSStruct] = locksweepWithBackoff(() => new Backoff())

  implicit val parallellocksweep: TwoVersionEngineImpl[LSStruct, ParallelLockSweep] = {
    val ex: Executor = Executors.newWorkStealingPool()
    new TwoVersionEngineImpl[LSStruct, ParallelLockSweep]("ParallelLockSweep", (engine, prior) => new ParallelLockSweep(new Backoff(), ex, engine, prior))
  }

  implicit val default: Engine[ParRP] = parrp

  def locksweepWithBackoff(backOff: () => Backoff): Engine[LSStruct]  = new TwoVersionEngineImpl[LSStruct, LockSweep]("LockSweep", (_, prior) => new LockSweep(backOff(), prior))
  def parrpWithBackoff(backOff: () => Backoff): Engine[ParRP] = new TwoVersionEngineImpl[ParRP, ParRP]("ParRP", (_, prior) => new ParRP(backOff(), prior))

}
