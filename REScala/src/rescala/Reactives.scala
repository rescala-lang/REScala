package rescala

import java.util.UUID

import rescala.log.ReactiveLogging
import rescala.propagation._

/** A Reactive is a value type which has a dependency to other Reactives */
trait Reactive {

  val id: UUID = UUID.randomUUID()

  def dependants(implicit turn: Turn): Set[Reactive]

  protected[this] var levels: Map[Turn, Int] = Map().withDefaultValue(0)

  def ensureLevel(newLevel: Int)(implicit turn: Turn): Boolean =
    if (levels(turn) < newLevel) {
      levels += turn -> newLevel
      true
    }
    else false

  def level(implicit turn: Turn): Int = levels(turn)

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult

  /** called to finalize the pulse value (turn commits) */
  protected[rescala] def commit(implicit turn: Turn): Unit = {
    levels = levels.withDefaultValue(math.max(levels(turn), levels.default(turn)))
    levels -= turn
  }
}

/** A node that has nodes that depend on it */
trait Dependency[+P] extends Reactive {

  private var _dependants: Map[Turn, Set[Reactive]] = Map().withDefaultValue(Set())

  final override def dependants(implicit turn: Turn): Set[Reactive] = _dependants(turn)

  def addDependant(dep: Reactive)(implicit turn: Turn): Unit = {
    _dependants += turn -> (_dependants(turn) + dep)
    turn.changed(this)
  }

  def removeDependant(dep: Reactive)(implicit turn: Turn) = {
    _dependants += turn -> (_dependants(turn) - dep)
    turn.changed(this)
  }

  private[this] var pulses: Map[Turn, Pulse[P]] = Map()

  def pulse(implicit turn: Turn): Pulse[P] = pulses.getOrElse(turn, NoChangePulse)

  final protected[this] def pulse(pulse: Pulse[P])(implicit turn: Turn): Unit = pulses += turn -> pulse

  override def commit(implicit turn: Turn): Unit = {
    pulses -= turn
    _dependants = _dependants.withDefaultValue(_dependants(turn))
    _dependants -= turn
    super.commit
  }

}

/** A node that depends on other nodes */
trait Dependant extends Reactive {
  
  protected[this] def staticDependencies(dependencies: Set[Dependency[_]])(implicit turn: Turn) = {
    ensureLevel(dependencies.map(_.level).max + 1)
    dependencies.foreach(_.addDependant(this))
    turn.changed(this)
  }
  
  private var dependencies: Map[Turn, Set[Dependency[_]]] = Map().withDefaultValue(Set())

  def addDependency(dep: Dependency[_])(implicit turn: Turn): Unit = {
    ensureLevel(dep.level + 1)
    dependencies += turn -> (dependencies(turn) + dep)
    dep.addDependant(this)
  }

  def setDependencies(newDependencies: Set[Dependency[_]])(implicit turn: Turn): Unit = {
    val oldDependencies = dependencies(turn)
    val removed = oldDependencies.diff(newDependencies)
    val added = newDependencies.diff(oldDependencies)
    removed.foreach(removeDependency)
    added.foreach(addDependency)
    dependencies += turn -> newDependencies
  }

  def removeDependency(dep: Dependency[_])(implicit turn: Turn): Unit = {
    dep.removeDependant(this)
    dependencies += turn -> (dependencies(turn) - dep)
  }

  override def commit(implicit turn: Turn): Unit = {
    dependencies = dependencies.withDefaultValue(dependencies(turn))
    dependencies -= turn
    super.commit
  }
}


