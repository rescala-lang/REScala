package rescala.graph

import rescala.synchronization.TurnLock
import rescala.turns.Engine
import rescala.turns.Turn

/** helper class to initialise engine and select lock */
abstract class Enlock(final override protected[rescala] val engine: Engine[Turn],
                      knownDependencies: Set[Reactive] = Set.empty) extends Reactive {
  final override protected[rescala] val lock: TurnLock =
    if (knownDependencies.size == 1)
       knownDependencies.head.lock
    else 
       new TurnLock(this)

  
  val staticIncoming: Set[Reactive] = knownDependencies
}

/**
 * Class which implements the TurnData for ReactiveTurnData
 */
abstract class ReactiveImpl(engine: Engine[Turn],
                      knownDependencies: Set[Reactive] = Set.empty)
   extends Enlock(engine, knownDependencies) {
  
  protected [this] override type Frame = ReactiveFrame
  
  protected override def initialStableFrame : ReactiveFrame = {
    new ReactiveFrame(null, this, knownDependencies);
  }
  
  protected override def newFrameFrom(turn: Turn, other: ReactiveFrame) : ReactiveFrame = {
    new ReactiveFrame(turn, this, 
        engine.buffer(other.level.get(turn), math.max, lock),
        engine.buffer(other.outgoing.get(turn), Buffer.commitAsIs, lock),
        other.incoming)
  }
  
}

/**
 * Class which implements the TurnData for PulsingTurnData
 */
abstract class PulsingImpl[+T](engine: Engine[Turn], knownDependencies: Set[Reactive] = Set.empty) 
    extends Enlock(engine, knownDependencies) with Pulsing[T] {
    protected [this] override type Frame = PulsingFrame[T]
    protected [this] override def initialStableFrame : PulsingFrame[T] = {
      new PulsingFrame(null, this, knownDependencies)
    }
    protected [this] override def newFrameFrom(turn : Turn, other : PulsingFrame[T]) : PulsingFrame[T] = {
      val newPulseBuffer = engine.buffer(other.pulses.get(turn), Buffer.transactionLocal[Pulse[T]], lock)
      new PulsingFrame[T](turn, this, 
          engine.buffer(other.level.get(turn), math.max,lock),
          engine.buffer(other.outgoing.get(turn), Buffer.commitAsIs,lock),
          other.incoming,
          newPulseBuffer)
    }
  }


/**
 * Class which implements the TurnData for StatefulTurnData
 */
abstract class StatefulImpl[+T](engine: Engine[Turn], knownDependencies: Set[Reactive] = Set.empty) 
    extends Enlock(engine, knownDependencies) with Stateful[T] {
    protected [this] override type Frame = StatefulFrame[T]
    protected [this] override def initialStableFrame : StatefulFrame[T] = {
      new StatefulFrame(null, this, knownDependencies)
    }
    protected [this] override def newFrameFrom(turn : Turn, other : StatefulFrame[T]) : StatefulFrame[T] = {
      val newPulseBuffer = engine.buffer(other.pulses.get(turn), Buffer.transactionLocal[Pulse[T]], lock)
      new StatefulFrame[T](turn, this, 
          engine.buffer(other.level.get(turn), math.max,lock),
          engine.buffer(other.outgoing.get(turn), Buffer.commitAsIs,lock),
          other.incoming,
          newPulseBuffer)
    }
  }

