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
  private object activeTurnsLock

  /**
   * A Map which stores for a mapping (t1, t2) -> rs, that
   * turn t1 is before turn t2 at the reactives rs
   */
  // TODO need to cleanup the map if turns are done
  private object graphLock

  private var completedNotRemovedTurns: Set[PTurn] = Set()
  private object completedNotRemovedTurnsLock

  protected[pipelining] def isActive(turn: PTurn) = activeTurnsLock.synchronized(activeTurns.contains(turn))

  protected def makeNewTurn = new PipeliningTurn(this)

  // For debugging
  override final protected[pipelining] def makeTurn: PipeliningTurn = {
    val newTurn = makeNewTurn
    activeTurnsLock.synchronized(activeTurns += newTurn)
    newTurn
  }

  protected[pipelining] def graphLocked[T](op: => T): T = graphLock.synchronized {
    op
  }

  private def rememberTurnOrder(newFrameTurn: PTurn, at: Reactive) = {
    var frameForNewTurnSeen = false
    at.foreachFrameTopDown(frame => {
      val currentTurn = frame.turn.asInstanceOf[PipeliningTurn]
      if (currentTurn == newFrameTurn) {
        assert(!frameForNewTurnSeen)
        frameForNewTurnSeen = true
      } else {
        if (frameForNewTurnSeen)
          rememberOrder(before = newFrameTurn, after = currentTurn, at)
        else
          rememberOrder(before = currentTurn, after = newFrameTurn, at)
          
      }
    })
  }

  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrame(turn: PTurn, at: Reactive) = graphLock.synchronized {
    at.createFrame(turn)
    rememberTurnOrder(turn, at)
    assert(assertCycleFree, "Create frame created a cycle")
  }
  
  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrameBefore(turn: PTurn, before : Set[PTurn], at: Reactive) = graphLock.synchronized {
    at.createFrameBefore(turn => before.contains(turn.asInstanceOf[PTurn]))(turn)
    rememberTurnOrder(turn, at)
    assert(assertCycleFree, "Create frame created a cycle")
  }

  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createDynamicReadFrameFrame(turn: PTurn, from: Reactive, at: Reactive) = graphLock.synchronized {
    val frame = at.createDynamicReadFrame(from)(turn)
    rememberTurnOrder(turn, at)
    assert(assertCycleFree, "Create dynamic read frame created a cycle")
    frame
  }

  protected[pipelining] def createFrameAfter(turn: PTurn, createFor: PTurn, at: Reactive): Boolean = graphLock.synchronized {
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

  private def putInMap[K, V](map: Map[K, Set[V]], key: K, v: V): Map[K, Set[V]] = {
    val vals = map.getOrElse(key, Set()) + v
    map + (key -> vals)
  }

  private def removeFromMap[K, V](map: Map[K, Set[V]], key: K, v: V): Map[K, Set[V]] = {
    val vals = map.getOrElse(key, Set()) - v
    if (vals.isEmpty)
      map - key
    else
      map + (key -> vals)
  }

  private def assertCycleFree() = {

    def bfsCycleCheck(turn: PipeliningTurn) = {

      var queue: Queue[PipeliningTurn] = Queue(turn)
      var seen: Set[PipeliningTurn] = Set(turn)
      var cycle = false

      while (!cycle && !queue.isEmpty) {
        val head = queue.head
        queue = queue.tail

        cycle = !head.preceedingTurns.forall {
          t =>
            assert(activeTurns.contains(t))
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
    true
  }

  private def rememberOrder(before: PTurn, after: PTurn, at: Reactive) = {
    assert(isActive(before), s"A frame from the already completed turn $before remained")
    after.preceedingTurns += before
    after.causedReactives = putInMap(after.causedReactives, before, at)
  }

  /**
   * Implements a depth first search of the waiting graph to check
   * whether waits waits on on
   */
  protected[pipelining] def waitsOn(waits: PTurn, on: PTurn): Boolean = {
    assert(activeTurns.contains(waits))
    assert(activeTurns.contains(on))
    assert(assertCycleFree())

    var queue: Queue[PTurn] = Queue(waits)
    var seen: Set[PTurn] = Set(waits)
    var found = false
    while (!found && !queue.isEmpty) {
      val head = queue.head
      queue = queue.tail
      seen += head
      if (head == on)
        found = true
      else {
        val newNodes = head.preceedingTurns -- seen
        queue ++= newNodes
      }

    }
    val isWaiting = found; //depthFirstSearch(waits, on)
    isWaiting
  }

  protected[pipelining] def turnCompleted(completedTurn: PTurn): Unit = {
    graphLock.synchronized {
      val turnRemoved = {
        val waitsOnAnyTurn = completedTurn.preceedingTurns.nonEmpty
        if (!waitsOnAnyTurn) {
          // TODO Maybe we can do this a bit more efficient
          activeTurns.foreach { noLongerWaitingTurn =>
            noLongerWaitingTurn.preceedingTurns -= completedTurn
            noLongerWaitingTurn.causedReactives -= completedTurn
          }
          // Remove all frames
          completedTurn.framedReactives.get.foreach { _.removeFrame(completedTurn) }
          activeTurnsLock.synchronized {
            assert(activeTurns.contains(completedTurn))
            activeTurns -= completedTurn
          }
          true
        } else {
          false
        }
      }
      completedNotRemovedTurnsLock.synchronized {
        if (!turnRemoved) {
          completedNotRemovedTurns += completedTurn
        } else {
          // Try to remove all turns again, because there is a turn removed
          val oldTurns = completedNotRemovedTurns;
          completedNotRemovedTurns = Set()
          oldTurns.foreach(turnCompleted(_))
        }
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