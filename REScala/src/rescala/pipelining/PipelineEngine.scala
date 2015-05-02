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
  

  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrame(turn: PTurn, at: Reactive) = graphLock.synchronized {
    // TODO first check for conflicts
    resolveConflicts(turn, at.getPipelineFrames().map { _.turn.asInstanceOf[PipeliningTurn] }.toSet)
    at.createFrame { frame =>
      rememberOrder(frame.turn.asInstanceOf[PipeliningTurn], turn, at)
    }(turn)
    assert(assertCycleFree, "Create frame created a cycle")
  }
  
  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createDynamicReadFrameFrame(turn: PTurn, from: Reactive,  at: Reactive) = graphLock.synchronized {
    val frame = at.createDynamicReadFrame(from, { frame =>
      rememberOrder(frame.turn.asInstanceOf[PipeliningTurn], turn, at)
    })(turn)
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
      at.insertWriteFrameFor(createFor, { frame =>
        val before = frame.turn.asInstanceOf[PipeliningTurn]
        assert(isActive(before), s"A frame from the already completed turn $before remained")
        // Then remember the new turn
        println("Remember order")
        rememberOrder(before, turn, at)
      })(turn)
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
  }

  private def forgetOrder(before: PTurn, after: PTurn, at: Reactive) = {
    after.causedReactives = removeFromMap(after.causedReactives, before, at)
    if (!after.causedReactives.contains(before)) {
      after.preceedingTurns -= before
    }
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
    var found = true
    while (!found && queue.isEmpty) {
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
          completedTurn.framedReactives.foreach { _.removeFrame(completedTurn) }
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

  private def allTurnsInAllPaths(from: PTurn, to: PTurn): Set[PTurn] = {
    assert(assertCycleFree())
    var markedSuccessful = Set[PTurn]()
    var markedUnsuccessful = Set[PTurn]()
    def findPaths(path: List[PTurn], currentNode: PTurn, indent: String): Set[PTurn] = {
      if (currentNode == to) {
        markedSuccessful ++= path
        path.toSet
      } else {
        val outgoings = currentNode.preceedingTurns;
        val alreadySuccessfulNodes = outgoings.intersect(markedSuccessful)
        val alreadyUnsuccessfulNodes = outgoings.intersect(markedUnsuccessful)
        val newNodes = outgoings -- alreadySuccessfulNodes -- alreadyUnsuccessfulNodes

        if (newNodes.isEmpty) {
          if (alreadySuccessfulNodes.nonEmpty) {
            markedSuccessful += currentNode
            path.toSet
          } else {
            markedUnsuccessful += currentNode
            Set()
          }
        } else {
          val newPathsNewNodes = newNodes.flatMap { newNode => findPaths(path :+ currentNode, newNode, indent + " ") }
          if (newPathsNewNodes.isEmpty) {
            if (alreadySuccessfulNodes.isEmpty) {
              markedUnsuccessful += currentNode
            } else {
              markedSuccessful += currentNode
            }
          } else {
            markedSuccessful += currentNode
          }
          newPathsNewNodes
        }

      }
    }
    val turns = findPaths(List(), from, "")
    turns
  }

  private def reactivesForConflicts(active: PTurn, conflicts: Set[PTurn]): Map[Reactive, Set[PTurn]] = {
    val reactivesForConflicts: Map[PTurn, Set[Reactive]] = conflicts.map { conflict =>
      val affectedReactives = conflict.causedReactives.getOrElse(active, Set())
      (conflict, affectedReactives)
    }.filter({ case (_, reactives) => reactives.nonEmpty }).toMap
    val allReactives = reactivesForConflicts.values.flatMap(x => x)
    val turnsForReactives: Map[Reactive, Set[PTurn]] = allReactives.map { reactive =>
      val turnsAtReactive = reactivesForConflicts.filter({ case (_, reactives) => reactives.contains(reactive) }).keys
      (reactive, turnsAtReactive.toSet)
    }.toMap
    turnsForReactives
  }

  private def repairReactive(reactive: Reactive, active: PTurn, conflicts: Set[PTurn]): Unit = {
    var seenTurns = Set[PTurn]()
    reactive.moveFrameBack { frame =>
      val turn = frame.turn.asInstanceOf[PipeliningTurn]
      forgetOrder(active, turn, reactive)
      rememberOrder(turn, active, reactive)
      seenTurns += turn
      conflicts.subsetOf(seenTurns)
    }(active)
  }

  private def resolveConflicts(active: PTurn, turnsAtReactive: Set[PTurn]): Unit = {
    val allConflictedTurns = turnsAtReactive.flatMap { allTurnsInAllPaths(_, active) }
    val affectedReactives = reactivesForConflicts(active, allConflictedTurns)
    affectedReactives.foreach { case (reactive, conflicts) => repairReactive(reactive, active, conflicts) }
    assert(assertCycleFree)

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