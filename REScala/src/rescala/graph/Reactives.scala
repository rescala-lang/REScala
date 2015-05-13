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

}

/**
 * Class which implements the TurnData for PulsingTurnData
 */
abstract class PulsingImpl[+T](engine: Engine[Turn], knownDependencies: Set[Reactive] = Set.empty)
  extends Enlock(engine, knownDependencies) with Pulsing[T] {

}

/**
 * Class which implements the TurnData for StatefulTurnData
 */
abstract class StatefulImpl[+T](engine: Engine[Turn], knownDependencies: Set[Reactive] = Set.empty)
  extends Enlock(engine, knownDependencies) with Stateful[T] {
  
}

