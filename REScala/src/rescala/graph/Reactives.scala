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
  
  protected [this] override type D = ReactiveTurnData
  
  protected override def initialStableFrame : ReactiveTurnData = {
    new ReactiveTurnData(None, this, knownDependencies);
  }
  
  protected override def newFrameFrom(turn: Turn, other: ReactiveTurnData) : ReactiveTurnData = {
    new ReactiveTurnData(Some(turn), this, 
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
    protected [this] override type D = PulsingTurnData[T]
    protected [this] override def initialStableFrame : PulsingTurnData[T] = {
      new PulsingTurnData(None, this, knownDependencies)
    }
    protected [this] override def newFrameFrom(turn : Turn, other : PulsingTurnData[T]) : PulsingTurnData[T] = {
      val newPulseBuffer = engine.buffer(other.pulses.get(turn), Buffer.transactionLocal[Pulse[T]], lock)
      new PulsingTurnData[T](Some(turn), this, 
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
    protected [this] override type D = StatefulTurnData[T]
    protected [this] override def initialStableFrame : StatefulTurnData[T] = {
      new StatefulTurnData(None, this, knownDependencies)
    }
    protected [this] override def newFrameFrom(turn : Turn, other : StatefulTurnData[T]) : StatefulTurnData[T] = {
      val newPulseBuffer = engine.buffer(other.pulses.get(turn), Buffer.transactionLocal[Pulse[T]], lock)
      new StatefulTurnData[T](Some(turn), this, 
          engine.buffer(other.level.get(turn), math.max,lock),
          engine.buffer(other.outgoing.get(turn), Buffer.commitAsIs,lock),
          other.incoming,
          newPulseBuffer)
    }
  }

