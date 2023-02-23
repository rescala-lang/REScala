package rescala

import rescala.Schedulers.Unmanaged
import rescala.Schedulers.unmanaged.bundle
import rescala.core.{AdmissionTicket, ReSource, Scheduler}
import rescala.interface.RescalaInterface
import rescala.scheduler.{Levelbased, Sidup, TopoBundle}

object Schedulers extends PlatformSchedulers {

  trait NoLock extends Levelbased {
    type State[V] = LevelState[V]
    private[rescala] class SimpleNoLock extends LevelBasedTransaction {
      override type State[V] = LevelState[V]
      override protected def makeDerivedStructState[V](initialValue: V): State[V] = new LevelState(initialValue)
      override def releasePhase(): Unit                                           = ()
      override def preparationPhase(initialWrites: Set[ReSource.of[State]]): Unit = {}
      override def beforeDynamicDependencyInteraction(dependency: ReSource): Unit = {}
    }
  }

  trait Unmanaged extends NoLock {
    val scheduler: Scheduler[State] =
      new TwoVersionScheduler[SimpleNoLock] {
        override protected def makeTransaction(priorTx: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock()
        override def schedulerName: String                                                  = "Unmanaged"
      }

  }

  trait Synchron extends NoLock {
    val scheduler: Scheduler[State] = new TwoVersionScheduler[SimpleNoLock] {
      override protected def makeTransaction(priorTx: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock
      override def schedulerName: String                                                  = "Synchron"
      override def forceNewTransaction[R](
          initialWrites: Set[ReSource.of[State]],
          admissionPhase: AdmissionTicket[State] => R
      ): R =
        synchronized { super.forceNewTransaction(initialWrites, admissionPhase) }
    }
  }

  /** Basic implementations of propagation engines */
  object unmanaged extends RescalaInterface {
    val bundle: Unmanaged = new Unmanaged {}
    type State[V] = bundle.State[V]
    def scheduler: Scheduler[State] = bundle.scheduler
  }

  object synchron extends Synchron with RescalaInterface

  object toposort extends RescalaInterface {
    val bundle: TopoBundle = new TopoBundle {}
    override type State[V] = bundle.State[V]
    override def scheduler: Scheduler[State] = bundle.TopoScheduler
  }

  object sidup extends Sidup with RescalaInterface {
    override type ReSource = rescala.core.ReSource.of[State]
    val scheduler: Scheduler[State] = new TwoVersionScheduler[SidupTransaction] {
      override protected def makeTransaction(priorTx: Option[SidupTransaction]): SidupTransaction = new SidupTransaction
      override def schedulerName: String                                                          = "SidupSimple"
      override def forceNewTransaction[R](
          initialWrites: Set[ReSource],
          admissionPhase: AdmissionTicket[State] => R
      ): R =
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
