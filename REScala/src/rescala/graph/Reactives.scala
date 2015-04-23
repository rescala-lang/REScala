package rescala.graph

import rescala.synchronization.TurnLock
import rescala.turns.Engine
import rescala.turns.Turn

/** helper class to initialise engine and select lock */
abstract class Enlock( final override protected[rescala] val engine: Engine[Turn],
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

  protected[this] override type Content = ReactiveFrame

  protected override def initialStableFrame: ReactiveFrame = {
    new ReactiveFrame(this, knownDependencies);
  }

  protected override def duplicate(other: ReactiveFrame): ReactiveFrame = {
    new ReactiveFrame(this,
      engine.buffer(other.level.get(null), math.max, lock),
      engine.buffer(other.outgoing.get(null), Buffer.commitAsIs, lock),
      other.incoming)
  }

}

/**
 * Class which implements the TurnData for PulsingTurnData
 */
abstract class PulsingImpl[+T](engine: Engine[Turn], knownDependencies: Set[Reactive] = Set.empty)
  extends Enlock(engine, knownDependencies) with Pulsing[T] {

  protected[this] override type Content = PulsingFrame[T]

  protected[this] override def initialStableFrame: PulsingFrame[T] = {
    new PulsingFrame(this, knownDependencies)
  }
  protected[this] override def duplicate(other: PulsingFrame[T]): PulsingFrame[T] = {
    val newPulseBuffer = engine.buffer(other.pulses.get(null), Buffer.transactionLocal[Pulse[T]], lock)
    new PulsingFrame[T](this,
      engine.buffer(other.level.get(null), math.max, lock),
      engine.buffer(other.outgoing.get(null), Buffer.commitAsIs, lock),
      other.incoming,
      newPulseBuffer)
  }
}

/**
 * Class which implements the TurnData for StatefulTurnData
 */
abstract class StatefulImpl[+T](engine: Engine[Turn], knownDependencies: Set[Reactive] = Set.empty)
  extends Enlock(engine, knownDependencies) with Stateful[T] {
  
  protected[this] override type Content = StatefulFrame[T]

  protected[this] override def initialStableFrame: StatefulFrame[T] = {
    new StatefulFrame(this, knownDependencies)
  }
  protected[this] override def duplicate(other: StatefulFrame[T]): StatefulFrame[T] = {
    val newPulseBuffer = engine.buffer(other.pulses.get(null), Buffer.transactionLocal[Pulse[T]], lock)
    //println (s"Create new frame for $turn at $this with pulses ${other.pulses.get(turn)}")
    new StatefulFrame[T](this,
      engine.buffer(other.level.get(null), math.max, lock),
      engine.buffer(other.outgoing.get(null), Buffer.commitAsIs, lock),
      other.incoming,
      newPulseBuffer)
  }
}

