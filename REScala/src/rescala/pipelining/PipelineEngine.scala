package rescala.pipelining

import rescala.turns.Engine
import rescala.turns.Engines.EngineImpl
import rescala.graph.Reactive
import rescala.synchronization.TurnLock
import rescala.graph.SimpleBuffer
import rescala.graph.Buffer
import rescala.turns.Turn
import scala.collection.immutable.Queue

/**
 * @author moritzlichter
 */
class PipelineEngine extends EngineImpl[PipeliningTurn]() {

  private type PTurn = PipeliningTurn

  private var activeTurns: Set[PTurn] = Set()
  private var turnOrder = List[PTurn]()
  private object activeTurnsLock
  private object turnOrderLock

  private var completedNotRemovedTurns: Set[PTurn] = Set()
  private object completedNotRemovedTurnsLock

  protected[pipelining] def isActive(turn: PTurn) = activeTurnsLock.synchronized(activeTurns.contains(turn))

  protected[pipelining] def addTurn(turn: PTurn) = turnOrderLock.synchronized(turnOrder :+= turn)

  protected def makeNewTurn = new PipeliningTurn(this)

  //For debugging
  protected[pipelining] def getActiveTurns() = activeTurns

  override final protected[pipelining] def makeTurn: PipeliningTurn = {
    val newTurn = makeNewTurn
    activeTurnsLock.synchronized(activeTurns += newTurn)
    newTurn
  }

  private def rememberTurnOrder(newFrameTurn: PTurn, at: Reactive) = {
    var frameForNewTurnSeen = false
    at.foreachFrameTopDown(frame => {
      val currentTurn = frame.turn.asInstanceOf[PipeliningTurn]
      if (currentTurn == newFrameTurn) {
        assert(!frameForNewTurnSeen)
        frameForNewTurnSeen = true
      } else {
        if (frameForNewTurnSeen) {
          assert(!waitsOn(newFrameTurn, currentTurn))
          rememberOrder(before = newFrameTurn, after = currentTurn, at)
        } else {
          assert(!waitsOn(currentTurn, newFrameTurn), s"Frame was not created with respect to order new=$newFrameTurn, old=$currentTurn, queue=${at.getPipelineFrames()}")
          rememberOrder(before = currentTurn, after = newFrameTurn, at)
        }

      }
    })
  }

  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrame(turn: PTurn, at: Reactive) = {
    at.createFrame(turn)
    rememberTurnOrder(turn, at)
    assert(assertCycleFree, "Create frame created a cycle")
  }

  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrameBefore(turn: PTurn, at: Reactive) = {
    at.createFrameBefore(otherTurn => otherTurn >>~ turn )(turn)
    rememberTurnOrder(turn, at)
    assert(assertCycleFree, "Create frame created a cycle")
  }

  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createDynamicReadFrameFrame(turn: PTurn, from: Reactive, at: Reactive) = {
    val frame = at.createDynamicReadFrame(from)(turn)
    rememberTurnOrder(turn, at)
    assert(assertCycleFree, "Create dynamic read frame created a cycle")
    frame
  }

  protected[pipelining] def createFrameAfter(turn: PTurn, createFor: PTurn, at: Reactive): Boolean = {
    // TODO first check for conflicts
    // resolveConflicts(turn, at.getPipelineFrames().map { _.turn.asInstanceOf[PipeliningTurn]}.toSet)
    if (at.hasFrame(createFor))
      // at has already a frame for createFor, dont create a new one
      // TODO assert that createFor is after turn in the pipeline
      false
    else {
      at.insertWriteFrameFor(createFor)(turn)
      rememberTurnOrder(createFor, at)
      assert(assertCycleFree, "Create frame created a cycle")
      true
    }
  }

  private def assertCycleFree() = {

    def bfsCycleCheck(turn: PipeliningTurn) = {

      var queue: Queue[PipeliningTurn] = Queue(turn)
      var seen: Set[PipeliningTurn] = Set(turn)
      var cycle = false

      while (!cycle && !queue.isEmpty) {
        val head = queue.head
        queue = queue.tail

        cycle = !head.preceedingTurns.get.forall {
          t =>
            // assert(activeTurns.contains(t))
            if (!seen.contains(t)) {
              queue = queue :+ t;
            }
            t != turn
        }
      }
      if (cycle) {
        println(s"Cycle on $turn")
        activeTurns.foreach { turn => println(s"$turn -> ${turn.preceedingTurns}") }
      }
      cycle
    }

    activeTurns.forall { !bfsCycleCheck(_) }
  }

  private def rememberOrder(before: PTurn, after: PTurn, at: Reactive) = {
    import rescala.util.JavaFunctionsImplicits._
    if (isActive(before)) {
      // this may add before altough if it is not active, because it may finish before the next line
      // but avoid synchronization and accept that, it does not matter later
      after.preceedingTurns.updateAndGet { turns: Set[PTurn] => turns + before }
    }
  }

  /**
   * Implements a depth first search of the waiting graph to check
   * whether waits waits on on
   */
  protected[pipelining] def waitsOn(waits: PTurn, on: PTurn): Boolean = {
    assert(activeTurns.contains(waits))
    assert(activeTurns.contains(on))

    turnOrderLock.synchronized {
      assert(turnOrder.contains(waits))
      assert(turnOrder.contains(on))
      turnOrder.indexOf(waits) >= turnOrder.indexOf(on)
    }

  }

  protected[pipelining] def turnCompleted(completedTurn: PTurn): Unit = {
    import rescala.util.JavaFunctionsImplicits._
    val turnRemoved = {
      activeTurnsLock.synchronized {
        assert(activeTurns.contains(completedTurn))
        val waitsOnAnyTurn = completedTurn.preceedingTurns.get.intersect(activeTurns).nonEmpty
        if (!waitsOnAnyTurn) {
          // Remove all frames
          completedTurn.framedReactives.get.foreach { _.removeFrame(completedTurn) }
          assert(completedTurn.framedReactives.get.forall { !_.hasFrame(completedTurn) })
          activeTurns -= completedTurn
          println(s"turn $completedTurn completed")
          println(s"Active turns $activeTurns")
          true
        } else {
          println(s"turn $completedTurn needs to wait")
          println(s"Active turns $activeTurns")
          false
        }
      }
    }
    completedNotRemovedTurnsLock.synchronized {
      if (!turnRemoved) {
        completedNotRemovedTurns += completedTurn
      } else {
        turnOrderLock.synchronized {
          def delete(list: List[PTurn]): List[PTurn] = list match {
            case head :: rest => if (head == completedTurn) rest else head :: delete(rest)
            case Nil          => List()
          }
          turnOrder = delete(turnOrder)
        }
        // Try to remove all turns again, because there is a turn removed
        val oldTurns = completedNotRemovedTurns;
        completedNotRemovedTurns = Set()
        oldTurns.foreach(turnCompleted(_))
      }
    }
  }

  // TODO remove synchronized
  class NoBuffer[A](initial: A) extends Buffer[A] {
    private var value = initial
    def initCurrent(value: A): Unit = synchronized { this.value = value }
    def initStrategy(strategy: (A, A) => A): Unit = {}

    def transform(f: (A) => A)(implicit turn: Turn): A = synchronized {
      value = f(value)
      value
    }
    def set(value: A)(implicit turn: Turn): Unit = synchronized {
      this.value = value
    }
    def base(implicit turn: Turn): A = synchronized { value }
    def get(implicit turn: Turn): A = synchronized { value }
    override def release(implicit turn: Turn): Unit = {}
    override def commit(implicit turn: Turn): Unit = {}
  }

  override def buffer[A](default: A, commitStrategy: (A, A) => A, writeLock: TurnLock): Buffer[A] = new NoBuffer(default)

}