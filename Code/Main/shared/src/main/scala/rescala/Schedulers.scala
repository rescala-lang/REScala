package rescala

import rescala.core.{AdmissionTicket, ReSource, Scheduler}
import rescala.interface.RescalaInterface
import rescala.scheduler.Levelbased

object Schedulers extends PlatformSchedulers {

  trait NoLock extends Levelbased {
    type State[V] = LevelState[V]
    private[rescala] class SimpleNoLock extends LevelBasedTransaction {
      type State[V] = LevelState[V]
      override protected def makeDerivedStructState[V](initialValue: V): State[V] = new LevelState(initialValue)
      override def releasePhase(): Unit                                           = ()
      override def preparationPhase(initialWrites: Set[ReSource.of[State]]): Unit           = {}
      override def beforeDynamicDependencyInteraction(dependency: ReSource): Unit = {}
    }

  }

  /** Basic implementations of propagation engines */
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
      override def forceNewTransaction[R](initialWrites: Set[ReSource.of[State]], admissionPhase: AdmissionTicket[State] => R): R =
        synchronized { super.forceNewTransaction(initialWrites, admissionPhase) }
    }
  }

  object unmanaged extends Unmanaged with RescalaInterface

  object synchron extends Synchron with RescalaInterface


  override def byName(name: String): RescalaInterface =
    name match {
      case "synchron"  => synchron
      case "unmanaged" => unmanaged
      case other       => super.byName(name)
    }

}
