package rescala

import java.util.concurrent.{Executor, Executors}

import rescala.core.{Scheduler, Struct}
import rescala.levelbased.LevelBasedPropagationEngines
import rescala.parrp._
import rescala.twoversion.TwoVersionSchedulerImpl

object Engines extends LevelBasedPropagationEngines {

  def byName[S <: Struct](name: String): Scheduler[S] = name match {
    case "synchron" => synchron.asInstanceOf[Scheduler[S]]
    case "unmanaged" => unmanaged.asInstanceOf[Scheduler[S]]
    case "parrp" => parrp.asInstanceOf[Scheduler[S]]
    case "locksweep" => locksweep.asInstanceOf[Scheduler[S]]

    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }
  def all: List[Scheduler[_ <: Struct]] = List(unmanaged, parrp, locksweep)

  implicit val parrp: Scheduler[ParRP] = parrpWithBackoff(() => new Backoff)

  implicit val locksweep: Scheduler[LSStruct] = locksweepWithBackoff(() => new Backoff())

  implicit val parallellocksweep: TwoVersionSchedulerImpl[LSStruct, ParallelLockSweep] = {
    val ex: Executor = Executors.newWorkStealingPool()
    new TwoVersionSchedulerImpl[LSStruct, ParallelLockSweep]("ParallelLockSweep", (engine, prior) => new ParallelLockSweep(new Backoff(), ex, engine, prior))
  }

  implicit val default: Scheduler[ParRP] = parrp

  def locksweepWithBackoff(backOff: () => Backoff): Scheduler[LSStruct]  = new TwoVersionSchedulerImpl[LSStruct, LockSweep]("LockSweep", (_, prior) => new LockSweep(backOff(), prior))
  def parrpWithBackoff(backOff: () => Backoff): Scheduler[ParRP] = new TwoVersionSchedulerImpl[ParRP, ParRP]("ParRP", (_, prior) => new ParRP(backOff(), prior))

}
