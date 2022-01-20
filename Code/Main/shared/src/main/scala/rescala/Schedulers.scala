package rescala

import rescala.interface.RescalaInterface
import rescala.scheduler.{Levelbased, Sidup, TopoBundle}

object Schedulers extends PlatformSchedulers {

  trait NoLock extends Levelbased {
    type State[V] = LevelState[V]
    private[rescala] class SimpleNoLock extends LevelBasedTransaction {
      override protected def makeDerivedStructState[V](initialValue: V): State[V] = new LevelState(initialValue)
      override def releasePhase(): Unit                                           = ()
      override def preparationPhase(initialWrites: Set[ReSource]): Unit           = {}
      override def beforeDynamicDependencyInteraction(dependency: ReSource): Unit = {}
    }

  }

  /** Basic implementations of propagation engines */
  trait Unmanaged extends NoLock {
    val scheduler: Scheduler =
      new TwoVersionScheduler[SimpleNoLock] {
        override protected def makeTransaction(priorTx: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock()
        override def schedulerName: String                                                  = "Unmanaged"
      }

  }

  trait Synchron extends NoLock {
    val scheduler: Scheduler = new TwoVersionScheduler[SimpleNoLock] {
      override protected def makeTransaction(priorTx: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock
      override def schedulerName: String                                                  = "Synchron"
      override def forceNewTransaction[R](initialWrites: Set[ReSource], admissionPhase: AdmissionTicket => R): R =
        synchronized { super.forceNewTransaction(initialWrites, admissionPhase) }
    }
  }

  object unmanaged extends Unmanaged with RescalaInterface

  object synchron extends Synchron with RescalaInterface

  object toposort extends TopoBundle with RescalaInterface {
    override def scheduler: toposort.Scheduler = TopoScheduler
    override type State[V] = TopoState[V]
    override def makeDerivedStructStateBundle[V](ip: V) = new TopoState(ip)
  }

  object sidup extends Sidup with RescalaInterface {
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
      case "toposort"  => toposort
      case "sidup"     => sidup
      case other       => super.byName(name)
    }

}
