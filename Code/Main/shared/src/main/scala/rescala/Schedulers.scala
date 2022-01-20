package rescala

import rescala.extra.scheduler.{Sidup, SimpleBundle}
import rescala.interface.RescalaInterface
import rescala.scheduler.{Synchron, Unmanaged}

object Schedulers extends PlatformSchedulers {

  object unmanaged extends Unmanaged with RescalaInterface

  object synchron extends Synchron with RescalaInterface

  object simple extends SimpleBundle with RescalaInterface {
    override def scheduler: simple.Scheduler = SimpleScheduler
    override type State[V] = SimpleState[V]
    override def makeDerivedStructStateBundle[V](ip: V) = new SimpleState(ip)
  }

  object sidupSimple extends Sidup with RescalaInterface {
    val scheduler: Scheduler = new TwoVersionScheduler[SidupTransaction] {
      override protected def makeTransaction(priorTx: Option[SidupTransaction]): SidupTransaction = new SidupTransaction
      override def schedulerName: String                                                          = "SidupSimple"
      override def forceNewTransaction[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R =
        synchronized { super.forceNewTransaction(initialWrites, admissionPhase) }
    }
  }

  override def byName(name: String): RescalaInterface =
    name match {
      case "synchron"  => synchron
      case "unmanaged" => unmanaged
      case "simple"    => simple
      case "sidup"     => sidupSimple
      case other       => super.byName(name)
    }

}
