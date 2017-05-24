package rescala.fullmv

import rescala.engine.EngineImpl
import rescala.fullmv.wsdeque.NonBlockingCircularArrayWSDeque

import scala.util.Try

object FullMVEngine extends EngineImpl[FullMVStruct, FullMVTurn] {
  val DEBUG = false

  val PROCESSORS = Runtime.getRuntime.availableProcessors()
  val workQueues = Array.fill(PROCESSORS) { new NonBlockingCircularArrayWSDeque[Task[FullMVTurn, Reactive]] }
  val entryWorkQueue = new NonBlockingCircularArrayWSDeque[Task[FullMVTurn, Reactive]]()
  val workerGroup = new ThreadGroup("FullMVWorkers")
  val workers = (0 until PROCESSORS).map { i =>
    val stealFrom = new Array[NonBlockingCircularArrayWSDeque[Task[FullMVTurn, Reactive]]](PROCESSORS)
    Array.copy(workQueues, i + 1, stealFrom, 0, PROCESSORS - i - 1)
    Array.copy(workQueues, 0, stealFrom, PROCESSORS - i - 1, i)
    stealFrom(PROCESSORS - 1) = entryWorkQueue
    val worker = new FullMVWorker(workerGroup, "FullMVWorker-"+i, workQueues(i), stealFrom)
    worker.start()
    worker
  }

  object sgt extends SerializationGraphTracking[FullMVTurn] {
    private var predecessors = Map[FullMVTurn, Set[FullMVTurn]]().withDefaultValue(Set())
    private var successors = Map[FullMVTurn, Set[FullMVTurn]]().withDefaultValue(Set())

    override def ensureOrder(defender: FullMVTurn, contender: FullMVTurn): OrderResult = synchronized {
      assert(defender != contender, s"cannot establish order between equal defender $defender and contender $contender")
      assert(defender.state > State.Initialized, s"$defender is not started and should thus not be involved in any operations")
      assert(contender.state > State.Initialized, s"$contender is not started and should thus not be involved in any operations")
      assert(contender.state < State.Completed, s"$contender cannot be a contender (already completed).")
      if(defender.state == State.Completed) {
        FirstFirst
      } else if (predecessors(defender)(contender)) {
        SecondFirst
      } else if (predecessors(contender)(defender)) {
        FirstFirst
      } else if(defender.state < contender.state) {
        establishOrder(contender, defender)
        SecondFirst
      } else {
        establishOrder(defender, contender)
        FirstFirst
      }
    }

    private def establishOrder(first: FullMVTurn, second: FullMVTurn): Unit = {
      val allAfter = successors(second) + second
      val allBefore = predecessors(first) + first
      for(succ <- allAfter) predecessors += succ -> (predecessors(succ) ++ allBefore)
      for(pred <- allBefore) successors += pred -> (successors(pred) ++ allAfter)
    }

    override def getOrder(found: FullMVTurn, searcher: FullMVTurn): PartialOrderResult = synchronized {
      assert(found != searcher, s"$found compared with itself..?")
      assert(found.state > State.Initialized, s"$found is not started and should thus not be involved in any operations")
      assert(searcher.state > State.Initialized, s"$searcher is not started and should thus not be involved in any operations")
      assert(searcher.state < State.Completed, s"$searcher cannot be a searcher (already completed).")
      if(found.state == State.Completed) {
        FirstFirst
      } else if (searcher.state == State.Completed) {
        SecondFirst
      } else if (predecessors(found)(searcher)) {
        SecondFirst
      } else if (predecessors(searcher)(found)) {
        FirstFirst
      } else {
        Unordered
      }
    }

    def discard(turn: FullMVTurn): Unit = synchronized {
      assert(turn.state == State.Completed, s"Trying to discard incomplete $turn")
      for(succ <- successors(turn)) {
        val previousPredecessors = predecessors(succ)
        if(previousPredecessors.size == 1) {
          assert(previousPredecessors == Set(turn), s"predecessor tracking for $succ was inaccurate: should only contain $turn, but contained $previousPredecessors.")
          predecessors -= succ
        } else {
          val remainingPredecessors = predecessors(succ) - turn
          predecessors += succ -> remainingPredecessors
        }
      }
      successors -= turn
    }

    override def awaitAllPredecessorsState(turn: FullMVTurn, atLeast: State.Type): Unit = {
      // Note that each turn on which this suspends has the opportunity to add additional predecessors to this turn
      // transitively. We do, however, not need to repeatedly lookup the set of all predecessors, thereby ignoring these,
      // because the turn on which this suspended will hold the suspension until these new transitive predecessors have
      // reached the same stage first. Thus, looking up our own predecessors again after a suspension might reveal
      // additional predecessors, but they would all have reached the required state already.
      sgt.predecessors(turn).foreach {
        _.awaitState(atLeast)
      }
    }
  }

  override protected def makeTurn(initialWrites: Traversable[Reactive], priorTurn: Option[FullMVTurn]): FullMVTurn = new FullMVTurn(sgt)
  override protected def executeInternal[I, R](turn: FullMVTurn, initialWrites: Traversable[Reactive], admissionPhase: () => I, wrapUpPhase: I => R): R = {
    // framing start
    turn.beginPhase(State.Framing, initialWrites.size)
    entryWorkQueue.synchronized {
      for(i <- initialWrites) entryWorkQueue.pushBottom(new Framing(turn, i))
    }

    // framing completion
    turn.awaitBranches()
    sgt.awaitAllPredecessorsState(turn, State.Executing)

    // admission
    turn.beginPhase(State.Executing, initialWrites.size)
    val admissionResult = Try(admissionPhase())

    // propagation start
    entryWorkQueue.synchronized {
      for(i <- initialWrites) entryWorkQueue.pushBottom(new Notification(turn, i, changed = admissionResult.isSuccess))
    }

    // propagation completion
    sgt.awaitAllPredecessorsState(turn, State.WrapUp)
    turn.awaitBranches()

    // wrap-up
    turn.beginPhase(State.WrapUp, 0)
    val result = admissionResult.flatMap(i => Try { wrapUpPhase(i) })

    // turn completion
    sgt.awaitAllPredecessorsState(turn, State.Completed)
    turn.beginPhase(State.Completed, -1)
    sgt.discard(turn)

    // result
    result.get
  }

}
