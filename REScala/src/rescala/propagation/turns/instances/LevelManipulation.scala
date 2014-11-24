package rescala.propagation.turns.instances

import rescala.propagation.Reactive
import rescala.propagation.turns.Turn

import scala.annotation.tailrec

class LevelManipulation()(implicit val turn: Turn) {

  def ensureLevel(dependant: Reactive, dependencies: Set[Reactive]): Boolean =
    if (dependencies.nonEmpty) setLevelIfHigher(dependant, dependencies.map(_.level.get).max + 1)
    else false

  def setLevelIfHigher(reactive: Reactive, level: Int): Boolean = {
    reactive.level.transform { case x if x < level => level }
  }

  @tailrec
  final def floodLevel(reactives: Set[Reactive]): Unit =
    if (reactives.nonEmpty) {
      val reactive = reactives.head
      val level = reactive.level.get + 1
      val dependants = reactive.dependants.get
      val changedDependants = dependants.filter(setLevelIfHigher(_, level))
      floodLevel(reactives.tail ++ changedDependants)
    }

}
