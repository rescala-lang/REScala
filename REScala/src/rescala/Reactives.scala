package rescala

import java.util.UUID

import rescala.log.ReactiveLogging
import rescala.propagation._

/** A Reactive is a value type which has a dependency to other Reactives */
trait Reactive extends ReactiveLogging {

  val id: UUID = UUID.randomUUID()

  def dependants(implicit turn: Turn): Set[Reactive]

  protected[this] var levels: Map[Turn, Int] = Map().withDefaultValue(0)

  def ensureLevel(newLevel: Int)(implicit turn: Turn): Unit =
    if (levels(turn) < newLevel) {
      levels += turn -> newLevel
    }

  def level(implicit turn: Turn): Int = levels(turn)

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult

  /** called to finalize the pulse value (turn commits) */
  protected[rescala] def commit(implicit turn: Turn): Unit = {
    levels = levels.withDefaultValue(math.max(levels(turn), levels.default(turn)))
    levels -= turn
  }

  log.nodeCreated(this)
}

/** A node that has nodes that depend on it */
trait Dependency[+P] extends Reactive {
  private var _dependants: Set[Reactive] = Set()
  final override def dependants(implicit turn: Turn): Set[Reactive] = _dependants

  def addDependant(dep: Reactive)(implicit turn: Turn): Unit = {
    _dependants += dep
    log.nodeAttached(dep, this)
  }

  def removeDependant(dep: Reactive)(implicit turn: Turn) = _dependants -= dep

  private[this] var pulses: Map[Turn, Pulse[P]] = Map()

  def pulse(implicit turn: Turn): Pulse[P] = pulses.getOrElse(turn, NoChangePulse)

  final protected[this] def pulse(pulse: Pulse[P])(implicit turn: Turn): Unit = {
    pulses += turn -> pulse
    log.nodePulsed(this)
  }

  override def commit(implicit turn: Turn): Unit = {
    pulses -= turn
    super.commit
  }

}

/** A node that depends on other nodes */
trait DynamicDependant extends Reactive {
  private var dependencies: Set[Dependency[_]] = Set()

  def addDependency(dep: Dependency[_])(implicit turn: Turn): Unit = {
    if (!dependencies.contains(dep)) {
      ensureLevel(dep.level)
      dependencies += dep
      dep.addDependant(this)
    }
  }
  def setDependencies(deps: TraversableOnce[Dependency[_]])(implicit turn: Turn): Unit = {
    val newDependencies = deps.toSet
    val removed = dependencies.diff(newDependencies)
    val added = newDependencies.diff(dependencies)
    removed.foreach(removeDependency)
    added.foreach(addDependency)
    dependencies = deps.toSet
  }
  def removeDependency(dep: Dependency[_])(implicit turn: Turn): Unit = {
    dep.removeDependant(this)
    dependencies -= dep
  }
}

trait StaticDependant {
  this: Reactive =>
  protected[this] def staticDependencies(dependencies: Set[Dependency[_]])(implicit turn: Turn) = {
    ensureLevel(dependencies.map(_.level).max + 1)
    dependencies.foreach(_.addDependant(this))
  }
}


