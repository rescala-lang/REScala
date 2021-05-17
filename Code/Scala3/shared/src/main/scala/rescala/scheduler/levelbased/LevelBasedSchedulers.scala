package rescala.scheduler.levelbased

import rescala.scheduler.Levelbased


/** Basic implementations of propagation engines */
trait LevelBasedSchedulers extends Levelbased {
  type State[V] = LevelState[V]

  private[rescala] class SimpleNoLock extends LevelBasedTransaction {
    override protected def makeDerivedStructState[V](ip: V): State[V] = {
      new LevelState(ip)
    }
    override def releasePhase(): Unit = ()
    override def preparationPhase(initialWrites: Set[ReSource]): Unit = {}
    override def beforeDynamicDependencyInteraction(dependency: ReSource): Unit = {}
  }

  implicit val synchron: Scheduler = {
    new TwoVersionScheduler[SimpleNoLock] {
      override protected def makeTransaction(priorTx: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock
      override def schedulerName: String                                                  = "Synchron"
      override def forceNewTransaction[R](
          initialWrites: Set[ReSource],
          admissionPhase: AdmissionTicket => R
      ): R =
        synchronized { super.forceNewTransaction(initialWrites, admissionPhase) }
    }
  }

  implicit val unmanaged: Scheduler =
    new TwoVersionScheduler[SimpleNoLock] {
      override protected def makeTransaction(priorTx: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock()
      override def schedulerName: String                                                  = "Unmanaged"
    }

}

object LevelBasedSchedulers extends LevelBasedSchedulers
