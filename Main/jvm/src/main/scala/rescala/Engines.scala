package rescala

import rescala.core.{Scheduler, Struct}
import rescala.levelbased.LevelBasedPropagationEngines
import rescala.parrp._
import rescala.simpleprop.SimpleScheduler
import rescala.twoversion.TwoVersionSchedulerImpl

object Engines extends LevelBasedPropagationEngines {

  def byName[S <: Struct](name: String): Scheduler[S] = name match {
    case "synchron" => synchron.asInstanceOf[Scheduler[S]]
    case "unmanaged" => unmanaged.asInstanceOf[Scheduler[S]]
    case "parrp" => parrp.asInstanceOf[Scheduler[S]]
    case "simple" => simple.asInstanceOf[Scheduler[S]]

    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }
  def all: List[Scheduler[_ <: Struct]] = List(unmanaged, parrp)

  implicit val parrp: Scheduler[ParRP] = parrpWithBackoff(() => new Backoff)

  implicit val default: Scheduler[ParRP] = parrp

  implicit val simple: SimpleScheduler.type = SimpleScheduler

  def parrpWithBackoff(backOff: () => Backoff): Scheduler[ParRP] = new TwoVersionSchedulerImpl[ParRP, ParRP]("ParRP", (_, prior) => new ParRP(backOff(), prior))

}
