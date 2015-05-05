package rescala.pipelining

import rescala.graph.Reactive

object ParallelFrameCreator {

  private object turnOrderLock
  private object waitingTurnLock
  private var turnOrder = List[PipeliningTurn]()
  private var waitingTurnsMap = Map[Reactive, Set[PipeliningTurn]]()

  protected def addTurn(turn: PipeliningTurn) = turnOrderLock.synchronized { turnOrder :+= turn }

  protected def requireReactive[T](reactive: Reactive, turn: PipeliningTurn): Unit = {
    waitingTurnLock.synchronized {
      val waitingTurns = waitingTurnsMap.getOrElse(reactive, Set());
      waitingTurnsMap += (reactive -> (waitingTurns + turn))
    }
  }

  protected def waitForAndReleaseLock[T](reactive: Reactive, turn: PipeliningTurn)(op: => T): T = {
    val preceedingTurns = turnOrder.takeWhile { _ != turn }.toSet

    // Again start with busy waiting
    while (waitingTurnLock.synchronized { waitingTurnsMap(reactive).intersect(preceedingTurns).nonEmpty }) {}

    val result = op

    waitingTurnLock.synchronized {
      val waitingTurns = (waitingTurnsMap(reactive) - turn)
      if (waitingTurns.isEmpty) {
        waitingTurnsMap -= reactive
      } else {
        waitingTurnsMap += (reactive -> waitingTurns)
      }
    }

    result

  }

}

trait ParallelFrameCreator extends QueueBasedFrameCreator {

  self: PipeliningTurn =>

  override protected[this] def createFrames(initialWrites: List[Reactive]) = {
    ParallelFrameCreator.addTurn(this)

    var requiredReactives = Set[Reactive]()
    evaluateQueue(initialWrites) { reactive =>
      ParallelFrameCreator.requireReactive(reactive, this)
      requiredReactives += reactive
    }

    var framedReactives = Set[Reactive]()

    evaluateQueue(initialWrites) { reactive =>
      // Need to check whether the reactive has been required
      // If not dynamic dependencies added the reactive to the outgoings set,
      // but they take care that a frame is created
      if (requiredReactives.contains(reactive)) {
        ParallelFrameCreator.waitForAndReleaseLock(reactive, this) {
          println(s"Create frame for $this at $reactive")
          createFrame(reactive)
          framedReactives += reactive
        }
      }
    }

    framedReactives

  }

}