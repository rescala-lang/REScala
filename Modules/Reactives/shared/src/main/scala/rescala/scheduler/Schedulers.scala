package rescala.scheduler

import rescala.core.{AdmissionTicket, ReSource, Scheduler}
import rescala.operator.Interface
import rescala.scheduler.{Levelbased, Sidup, TopoBundle}

object Schedulers extends PlatformSchedulers {

  trait NoLock extends Levelbased {
    type State[V] = LevelState[V]
    private[rescala] class SimpleNoLock extends LevelBasedTransaction {
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
  object unmanaged extends Interface {
    val bundle: Unmanaged = new Unmanaged {}
    type State[V] = bundle.State[V]
    def scheduler: Scheduler[State] = bundle.scheduler
  }

  object synchron extends Interface {
    val bundle: Synchron = new Synchron {}
    type State[V] = bundle.State[V]
    def scheduler: Scheduler[State] = bundle.scheduler
  }

  object toposort extends Interface with TopoBundle {
    override def makeDerivedStructStateBundle[V](ip: V): TopoState[V] = new TopoState[V](ip)
    override type State[V] = TopoState[V]
    override def scheduler: Scheduler[State] = TopoScheduler
  }

  object sidup extends Interface {
    val bundle: Sidup = new Sidup {}
    override type State[V] = bundle.State[V]
    val scheduler: Scheduler[State] = new bundle.TwoVersionScheduler[bundle.SidupTransaction] {
      override protected def makeTransaction(priorTx: Option[bundle.SidupTransaction]): bundle.SidupTransaction =
        new bundle.SidupTransaction
      override def schedulerName: String = "SidupSimple"
      override def forceNewTransaction[R](
          initialWrites: Set[ReSource.of[State]],
          admissionPhase: AdmissionTicket[State] => R
      ): R =
        synchronized { super.forceNewTransaction(initialWrites, admissionPhase) }
    }
  }

  override def byName(name: String): Interface =
    name match {
      case "synchron"  => synchron
      case "unmanaged" => unmanaged
      case "toposort"  => toposort
      case "sidup"     => sidup
      case other       => super.byName(name)
    }

}
