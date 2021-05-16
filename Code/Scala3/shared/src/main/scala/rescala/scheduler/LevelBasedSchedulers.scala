//package rescala.scheduler

//import rescala.core.{AdmissionTicket, ReSource, Scheduler}
//import rescala.scheduler.twoversion.TwoVersionScheduler
//
///** Basic implementations of propagation engines */
//trait LevelBasedSchedulers {
//
//  private[rescala] class SimpleNoLock extends LevelBasedTransaction[LevelStructImpl] {
//    override protected def makeDerivedStructState[V](ip: V): LevelState[V, LevelStructImpl] = {
//      new LevelState(ip)
//    }
//    override def releasePhase(): Unit = ()
//    override def preparationPhase(initialWrites: Set[ReSource[LevelStructImpl]]): Unit = {}
//    override def beforeDynamicDependencyInteraction(dependency: ReSource[LevelStructImpl]): Unit = {}
//  }
//
//  implicit val synchron: Scheduler[LevelStructImpl] = {
//    new TwoVersionScheduler[LevelStructImpl, SimpleNoLock] {
//      override protected def makeTransaction(priorTx: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock
//      override def schedulerName: String                                                  = "Synchron"
//      override def forceNewTransaction[R](
//          initialWrites: Set[ReSource[LevelStructImpl]],
//          admissionPhase: AdmissionTicket[LevelStructImpl] => R
//      ): R =
//        synchronized { super.forceNewTransaction(initialWrites, admissionPhase) }
//    }
//  }
//
//  implicit val unmanaged: Scheduler[LevelStructImpl] =
//    new TwoVersionScheduler[LevelStructImpl, SimpleNoLock] {
//      override protected def makeTransaction(priorTx: Option[SimpleNoLock]): SimpleNoLock = new SimpleNoLock()
//      override def schedulerName: String                                                  = "Unmanaged"
//    }
//
//}
//
//object LevelBasedSchedulers extends LevelBasedSchedulers
