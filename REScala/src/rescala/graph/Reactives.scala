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
  
  protected [this] override type Frame = ReactiveFrameImpl
  
  protected override def initialStableFrame : ReactiveFrameImpl = {
    new ReactiveFrameImpl(null, this, knownDependencies);
  }
  
  protected override def newFrameFrom(turn: Turn, other: ReactiveFrameImpl) : ReactiveFrameImpl = {
    new ReactiveFrameImpl(turn, this, 
        engine.buffer(other.level.get(other.turn), math.max, lock),
        engine.buffer(other.outgoing.get(other.turn), Buffer.commitAsIs, lock),
        other.incoming)
  }
  
}

/**
 * Class which implements the TurnData for PulsingTurnData
 */
abstract class PulsingImpl[+T](engine: Engine[Turn], knownDependencies: Set[Reactive] = Set.empty) 
    extends Enlock(engine, knownDependencies) with Pulsing[T] {
    protected [this] override type Frame = PulsingFrameImpl[T]
    protected [this] override def initialStableFrame : PulsingFrameImpl[T] = {
      new PulsingFrameImpl(null, this, knownDependencies)
    }
    protected [this] override def newFrameFrom(turn : Turn, other : PulsingFrameImpl[T]) : PulsingFrameImpl[T] = {
      val newPulseBuffer = engine.buffer(other.pulses.get(other.turn), Buffer.transactionLocal[Pulse[T]], lock)
      new PulsingFrameImpl[T](turn, this, 
          engine.buffer(other.level.get(other.turn), math.max,lock),
          engine.buffer(other.outgoing.get(other.turn), Buffer.commitAsIs,lock),
          other.incoming,
          newPulseBuffer)
    }
  }


/**
 * Class which implements the TurnData for StatefulTurnData
 */
abstract class StatefulImpl[+T](engine: Engine[Turn], knownDependencies: Set[Reactive] = Set.empty) 
    extends Enlock(engine, knownDependencies) with Stateful[T] {
    protected [this] override type Frame = StatefulFrameImpl[T]
    protected [this] override def initialStableFrame : StatefulFrameImpl[T] = {
      new StatefulFrameImpl(null, this, knownDependencies)
    }
    protected [this] override def newFrameFrom(turn : Turn, other : StatefulFrameImpl[T]) : StatefulFrameImpl[T] = {
      val newPulseBuffer = engine.buffer(other.pulses.get(other.turn), Buffer.transactionLocal[Pulse[T]], lock)
      //println (s"Create new frame for $turn at $this with pulses ${other.pulses.get(turn)}")
      new StatefulFrameImpl[T](turn, this, 
          engine.buffer(other.level.get(other.turn), math.max,lock),
          engine.buffer(other.outgoing.get(other.turn), Buffer.commitAsIs,lock),
          other.incoming,
          newPulseBuffer)
    }
  }

