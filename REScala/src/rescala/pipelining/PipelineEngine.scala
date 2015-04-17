package rescala.pipelining

import rescala.turns.Engine
import rescala.turns.Engines.EngineImpl
import rescala.graph.Reactive
import rescala.synchronization.TurnLock
import rescala.graph.SimpleBuffer
import rescala.graph.Buffer
import rescala.turns.Turn

/**
 * @author moritzlichter
 */
class PipelineEngine extends EngineImpl[PipeliningTurn]() {

  private type PTurn = PipeliningTurn

  /**
   * A Map which stores for a mapping (t1, t2) -> rs, that
   * turn t1 is before turn t2 at the reactives rs
   */
  // TODO need to cleanup the map if turns are done
  private var ordering: Map[(PTurn, PTurn), Set[Reactive]] = Map().withDefaultValue(Set())
  private object graphLock
  
  /**
   * A map which tracks which turn waits for which other. If t1 -> ts,
   * then for all t in ts an entry (t1, t) in ordering exists, which
   * does not map to an empty set (or is not defined)
   */
  private var waitingEdges: Map[PTurn, Set[PTurn]] = Map()

  protected[pipelining] def getOrdering = ordering
  protected[pipelining] def getWaitingEdges = waitingEdges

  // For debugging
  override protected[pipelining] def makeTurn: PipeliningTurn = new PipeliningTurn(this)

  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrame(turn: PTurn, at: Reactive) = graphLock.synchronized{
    // TODO first check for conflicts
    at.createFrame { frame =>
      val before = frame.turn.asInstanceOf[PipeliningTurn]
      // First resolve conflicts which would create a cycle
      resolveConflicts(before, turn)
      // Then remember the new turn
      rememberOrder(before, turn, at)
      true
    }(turn)
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

  private def forgetOrder(before: PTurn, after: PTurn, at: Reactive) = {
    ordering = removeFromMap(ordering, (before, after), at)
    if (!ordering.contains((before, after))) {
      waitingEdges = removeFromMap(waitingEdges, after, before)
    }
  }

  private def rememberOrder(before: PTurn, after: PTurn, at: Reactive) = {
    ordering = putInMap(ordering, (before, after), at)
    waitingEdges = putInMap(waitingEdges, after, before)
  }

  /**
   * Implements a depth first search of the waiting graph to check
   * whether waits waits on on
   */
  protected[pipelining] def waitsOn(waits: PTurn, on: PTurn): Boolean = {
    if (waits == on)
      true
    else {
      waitingEdges.getOrElse(waits, Set()).exists { waitsOn(_, on) }
    }
  }

  /**
   * Calculates a list of conflicts which will occur if an waiting edge from after to before
   * is created.
   *
   * @return a list of pairs of: a turn, which is in conflict with after, and a set of
   *   reactives, at which the conflicts occur
   */
  private def getConflicts(before: PTurn, after: PTurn): List[(PTurn, Set[Reactive])] = {
    // if waitsOn(before, after) inserting an edge from after to before
    // would create a cycle

    // But checking waitsOn(before, after) does not help, because we need to know the
    // first node on the path (near to after)
    // So get all connected nodes to after and check waitsOn on them
    val predecessorsOfAfter = waitingEdges.keySet.filter { waitingEdges(_).contains(after) };
    val predecessorsInCycle = predecessorsOfAfter.filter { waitsOn(before, _) }.toList
    val conflictedReactives = predecessorsInCycle.map { before => ordering.getOrElse((after, before), Set()) }
    predecessorsInCycle.zip(conflictedReactives)
  }

  private def resolveConflicts(before: PTurn, after: PTurn) = {
    
    // Resolves all conflicts which occur by putting a waiting relation from after to before
   
    // Resolving terminates because only frames for after are put back at some reactives
    // All conflicts are guaranteed solved, if after is moved to the back at all reactives
    // But we dont do that, only there, were it is neceassary
   
    def resolveConflict(before: PTurn, after: PTurn, at: Reactive) : Set[PTurn] = {
      var skipedFrames = Set[PTurn]()
      at.moveFrameBack { frame =>
        val before2 = frame.turn.asInstanceOf[PTurn]
        forgetOrder(after, before2, at)
        rememberOrder(before2, after, at)
        if (waitsOn(before2, after))
          skipedFrames += before2
        before2 == before
      }(after)
      skipedFrames
    }
    def findAndResolveConflicts(before: PTurn, after : PTurn) : Unit= {
      val conflicts = getConflicts(before, after)
      val newConflictedTurns = conflicts.flatMap{ _ match {
        case (turn, reactives) =>
          val conflicts = reactives.flatMap { reactive =>
              resolveConflict(turn, after, reactive)
            }
          assert(!ordering.contains((after, turn)), "Created a cycle") 
          conflicts
        }
        
      }
      newConflictedTurns.foreach { findAndResolveConflicts(_, after) }
    }

    findAndResolveConflicts(before, after)
  }
  
  // TODO remove synchronized
  class NoBuffer[A](initial :A) extends Buffer[A] {
    private var value = initial
    def initCurrent(value: A): Unit = synchronized{ this.value = value}
    def initStrategy(strategy: (A, A) => A): Unit = {}

    def transform(f: (A) => A)(implicit turn: Turn): A = synchronized{
      value = f(value)
      value
    }
    def set(value: A)(implicit turn: Turn): Unit = synchronized{
      this.value = value
    }
    def base(implicit turn: Turn): A = synchronized{value}
    def get(implicit turn: Turn): A = synchronized{value}
    override def release(implicit turn: Turn): Unit = {}
    override def commit(implicit turn: Turn): Unit = {}
  }
  
  override def buffer[A](default: A, commitStrategy: (A, A) => A, writeLock: TurnLock): Buffer[A] = new NoBuffer(default)


}