package rescala.scheduler

trait NoLock extends Levelbased {
  type State[V] = LevelState[V]
  private[rescala] class SimpleNoLock extends LevelBasedTransaction {
    override protected def makeDerivedStructState[V](ip: V): State[V] = new LevelState(ip)
    override def releasePhase(): Unit                                 = ()
    override def preparationPhase(initialWrites: Set[ReSource]): Unit = {}
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
