package rescala.pipelining

import rescala.graph.Reactive
import rescala.turns.Turn
import rescala.propagation.TurnImpl
import rescala.graph.Committable
import rescala.propagation.LevelQueue
import rescala.graph.ReactiveFrame
import rescala.graph.ReactiveFrame
import rescala.Signal
import rescala.graph.Pulsing
import rescala.graph.ReevaluationResult._
import rescala.graph.{ DynamicReadFrame, DynamicDropFrame, WriteFrame }
import rescala.graph.DynamicReadFrame
import rescala.graph.Frame
import rescala.graph.ReactiveFrame
import rescala.graph.Framed

object PipeliningTurn {

  private object lockPhaseLock
}

class PipeliningTurn(override val engine: PipelineEngine, randomizeDeps: Boolean = false) extends TurnImpl {

  /**
   * Remember all reactives for which a frame was created during this turn
   */
  protected[pipelining] var framedReactives: Set[Reactive] = Set()
  protected[pipelining] var preceedingTurns: Set[PipeliningTurn] = Set()
  protected[pipelining] var causedReactives: Map[PipeliningTurn, Set[Reactive]] = Map()

  override def waitsOnFrame(other: Turn) = other == this || engine.waitsOn(this, other.asInstanceOf[PipeliningTurn])

  override def waitsOnLock[T](op: => T): T = engine.graphLocked(op)

  val allNodesQueue = new LevelQueue
  

  protected override def requeue(head: Reactive, changed: Boolean, level: Int, redo: Boolean): Unit ={
    if (redo)
      allNodesQueue.enqueue(level, changed)(head)
    else head.outgoing.get.foreach(allNodesQueue.enqueue(level, changed))
    super.requeue(head, changed, level, redo)
  }

  private def markUnreachablePrunedNodesWritten(head: Reactive) = {
    // In all queue are all nodes, not just the changed ones, for 
    allNodesQueue.processQueueUntil { allHead =>
      assert(allHead.hasFrame)
      if (allHead.level.get < head.level.get) {
        allHead.markWritten
        false
      } else {
        true
      }
    }
  }

  override def evaluate(head: Reactive) = {
    assert(head.hasFrame(this), "No frame was created in turn " + this + " for " + head)

    // Marks all nodes written, which cannot be reached anymore and which was pruned
    // in order not to block pipelining longer on these nodes
    markUnreachablePrunedNodesWritten(head)

    var previousDynamicFrameNeeded = false
    do {

      head.waitUntilCanWrite

      val dynamicFramesMap = head.getIncompleteDynamicFrames
      val reactives = dynamicFramesMap.keySet

      // for each reactive for which unhandled dynamic frames exists, they may be multiple
      // write frames, get the most if available and put a frame for this on this reactive
      val firstDynamicFramesRequringWrites = reactives.map { reactive =>
        val dynamicFrames = dynamicFramesMap(reactive)
        val fristDynamicFramesRequiringWriteFrame = dynamicFrames.find { dynamicFrame =>
          dynamicFrame match {
            case DynamicReadFrame(turn, at, newDependent) =>
              assert(at == reactive)
              assert(newDependent == head)
              val needNewWriteFrame = dynamicFrame.next() match {
                case WriteFrame(nextTurn, at) =>
                  assert(at == reactive)
                  if (this >~ nextTurn.asInstanceOf[PipeliningTurn]) {
                    // The other turn needs to be evaluated first
                    true
                  } else
                    false
                case _ => false
              }
              needNewWriteFrame
            case DynamicDropFrame(_, _, _) => false // TODO do that later
            case WriteFrame(_, _)          => throw new AssertionError("no write frmae should be there")
          }
        }

        (reactive, fristDynamicFramesRequiringWriteFrame)
      }.filter(!_._2.isEmpty)
      previousDynamicFrameNeeded = firstDynamicFramesRequringWrites.nonEmpty
      if (previousDynamicFrameNeeded) {
        val framesMap = firstDynamicFramesRequringWrites.map[(Framed, Frame[_]), Iterable[(Framed, Frame[_])]](pair => (pair._1, pair._2.get)).toMap
        val turnsMaps = framesMap.mapValues { _.next().turn.asInstanceOf[PipeliningTurn] }

        def earliestTurns(turns: Iterable[PipeliningTurn]): Iterable[PipeliningTurn] = {
          turns.filter { turn => !turns.exists { otherTurn => turn >~ otherTurn } }
        }

        engine.graphLocked {
          val firstTurns = earliestTurns(turnsMaps.values.toSet) // Convert to set to avoid duplicates
          val pickedTurn = firstTurns.find(_ => true).get //There is always a turn because >~ is a partial order and turnMaps not empty
          engine.createFrameBefore(this, pickedTurn, head)
          // TODO sync for that
          pickedTurn.framedReactives += head
          // Dynamic frames for all reactive with this frame as first one are handled
          val handledReactives = turnsMaps.filter(_._2 == pickedTurn).keys
          handledReactives.foreach { reactive => reactive.forgetDynamicFramesUntil(framesMap(reactive)) }
          // Put head in queue of the picked turn
          pickedTurn.admit(head)
        }

      }
    } while (previousDynamicFrameNeeded)

    head.markTouched

    assert(preceedingTurns.forall(turn =>
      head.findFrame {
        _ match {
          case None        => true
          case Some(frame) => frame.isWritten
        }
      }(turn)))

    super.evaluate(head)

    head.markWritten
  }

  override def register(sink: Reactive)(source: Reactive): Unit = {
    println(s"Create dynamic frome at $source for $sink")
    val readFrame = source.createDynamicReadFrame(from = sink)
    sink.registerDynamicFrame(readFrame)
    framedReactives += source
    super.register(sink)(source)
  }

  override def unregister(sink: Reactive)(source: Reactive): Unit = {
  //  val dropFrame = source.createDynamicDropFrame(sink)
  //  sink.registerDynamicFrame(dropFrame)
    super.unregister(sink)(source)
  }

  // lock phases cannot run in parrallel currently,......
  override def lockPhase(initialWrites: List[Reactive]): Unit = {
    PipeliningTurn.lockPhaseLock.synchronized {
      def createFrame(reactive: Reactive): Unit = {
        engine.createFrame(this, reactive)
        assert(reactive.hasFrame(this))
        framedReactives += reactive
      }

      val lq = new LevelQueue()
      initialWrites.foreach { createFrame(_) }
      initialWrites.foreach(lq.enqueue(-1))

      // Create frames for all reachable reactives
      lq.evaluateQueue { reactive =>
        val outgoings = reactive.outgoing.get
        outgoings.foreach(createFrame(_))
        outgoings.foreach { lq.enqueue(-1) }
      }
    }
  }

  override def releasePhase(): Unit = {
    framedReactives.foreach(_.markWritten)
    engine.turnCompleted(this)
  }

  override def toString = {
    s"PipeliningTurn(${super.toString})"
  }

  def >~(other: PipeliningTurn) = {
    engine.waitsOn(this, other)
  }

}