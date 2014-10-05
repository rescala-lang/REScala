package rescala.propagation

import rescala.propagation.Pulse.{Diff, NoChange}

/** A Reactive is a value type which has a dependency to other Reactives */
trait Reactive {

  private[this] var levels: TurnLocal[Int] = TurnLocal(0)

  private[this] var _dependants: TurnLocal[Set[Reactive]] = TurnLocal(Set())

  final def ensureLevel(newLevel: Int)(implicit turn: Turn): Boolean =
    if (levels.get < newLevel) {
      levels <<= newLevel
      true
    }
    else false

  final def level(implicit turn: Turn): Int = levels.get

  final def dependants(implicit turn: Turn): Set[Reactive] = _dependants.get

  final def addDependant(dep: Reactive)(implicit turn: Turn): Unit = {
    _dependants <<<= (_ + dep)
    turn.changed(this)
  }

  final def removeDependant(dep: Reactive)(implicit turn: Turn) = {
    _dependants <<<= (_ - dep)
    turn.changed(this)
  }

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[propagation] def reevaluate()(implicit turn: Turn): EvaluationResult

  /** called to finalize the pulse value (turn commits) */
  protected[propagation] def commit(implicit turn: Turn): Unit = {
    levels = levels.combineCommit(math.max)
    _dependants = _dependants.commit
  }
}

/** A node that has nodes that depend on it */
trait Pulsing[+P] extends Reactive {
  def pulse(implicit turn: Turn): Pulse[P]

  protected[this] def setPulse(pulse: Pulse[P])(implicit turn: Turn): Unit
}

trait Stateless[+P] extends Pulsing[P] {
  private[this] var pulses: TurnLocal[Pulse[P]] = TurnLocal(Pulse.none)

  def pulse(implicit turn: Turn): Pulse[P] = pulses.get

  final protected[this] def setPulse(pulse: Pulse[P])(implicit turn: Turn): Unit = pulses <<= pulse

  override def commit(implicit turn: Turn): Unit = {
    pulses = pulses.release
    super.commit
  }
}

/** a node that has a current state */
trait Stateful[+A] extends Pulsing[A] {
  protected[this] var pulses: TurnLocal[Pulse[A]]

  override def pulse(implicit turn: Turn): Pulse[A] = pulses.get

  final protected[this] def setPulse(pulse: Pulse[A])(implicit turn: Turn): Unit = pulses <<= pulse

  override def commit(implicit turn: Turn): Unit = {
    pulses = pulses.combineCommit { (_, p) => p.keep}
    super.commit
  }

  def get(implicit maybe: MaybeTurn): A = maybe { getValue(_) }

  def getValue(implicit turn: Turn): A = pulse match {
    case NoChange(Some(value)) => value
    case Diff(value, oldOption) => value
    case NoChange(None) => throw new IllegalStateException("stateful reactive has never pulsed")
  }
}


/** reevaluation strategy for static dependencies */
trait StaticReevaluation[+P] extends Pulsing[P] {
  /** side effect free calculation of the new pulse for the current turn */
  protected[propagation] def calculatePulse()(implicit turn: Turn): Pulse[P]

  final override protected[propagation] def reevaluate()(implicit turn: Turn): EvaluationResult = {
    val p = calculatePulse()
    setPulse(p)
    EvaluationResult.Done(p.isChange, dependants)
  }
}

/** reevaluation strategy for dynamic dependencies */
trait DynamicReevaluation[+P] extends Pulsing[P] {
  /** side effect free calculation of the new pulse and the new dependencies for the current turn */
  def calculatePulseDependencies(implicit turn: Turn): (Pulse[P], Set[Reactive])
  
  final override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult = {
    val (newPulse, newDependencies) = calculatePulseDependencies

    if (!turn.isReady(this, newDependencies)) {
      EvaluationResult.Retry(newDependencies)
    }
    else {
      setPulse(newPulse)
      EvaluationResult.Done(newPulse.isChange, dependants, newDependencies)
    }
  }
}


/** A node that depends on other nodes */
class Dependencies(reactive: Reactive) {
  private var dependencies: Map[Turn, Set[Pulsing[_]]] = Map().withDefaultValue(Set())

  def addDependency(dep: Pulsing[_])(implicit turn: Turn): Unit = {
    dependencies += turn -> (dependencies(turn) + dep)
    dep.addDependant(reactive)
  }

  def setDependencies(newDependencies: Set[Pulsing[_]])(implicit turn: Turn): Unit = {
    val oldDependencies = dependencies(turn)
    val removed = oldDependencies.diff(newDependencies)
    val added = newDependencies.diff(oldDependencies)
    removed.foreach(removeDependency)
    added.foreach(addDependency)
    dependencies += turn -> newDependencies
  }

  def removeDependency(dep: Pulsing[_])(implicit turn: Turn): Unit = {
    dep.removeDependant(reactive)
    dependencies += turn -> (dependencies(turn) - dep)
  }

  def commit(implicit turn: Turn): Unit = {
    dependencies = dependencies.withDefaultValue(dependencies(turn))
    dependencies -= turn
  }
}


