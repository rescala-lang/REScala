package rescala

import rescala.core.{Scheduler, Struct}
import rescala.interface.RescalaInterface
import rescala.levelbased.LevelBasedSchedulers
import rescala.parrp._
import rescala.extra.simpleprop.SimpleScheduler
import rescala.twoversion.TwoVersionScheduler

object Schedulers extends LevelBasedSchedulers {

  def byName[S <: Struct](name: String): Scheduler[S] = name match {
    case "synchron" => synchron.asInstanceOf[Scheduler[S]]
    case "unmanaged" => unmanaged.asInstanceOf[Scheduler[S]]
    case "parrp" => parrp.asInstanceOf[Scheduler[S]]
    case "simple" => simple.asInstanceOf[Scheduler[S]]
    case other => throw new IllegalArgumentException(s"unknown engine $other")
  }

  implicit val parrp: Scheduler[ParRPStruct] = parrpWithBackoff(() => new Backoff)

  implicit val simple: SimpleScheduler.type = SimpleScheduler

  def parrpWithBackoff(backOff: () => Backoff): Scheduler[ParRPStruct] =
    new TwoVersionScheduler[ParRPStruct, ParRPTransaction] {
      override protected def makeTurn(priorTurn: Option[ParRPTransaction]): ParRPTransaction = new ParRPTransaction(backOff(), priorTurn)
      override def schedulerName: String = "ParRP"
    }
}

object Interfaces {
    val parrp = RescalaInterface.interfaceFor(Schedulers.parrp)
}
