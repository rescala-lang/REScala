package rescala.propagation.turns.instances

import rescala.propagation.Reactive

class DependencyManagement()(implicit val turn: AbstractTurn) {

  def register(dependant: Reactive)(dependency: Reactive): Unit = {
    dependency.dependants.transform(_ + dependant)
  }

  def unregister(dependant: Reactive)(dependency: Reactive): Unit = {
    turn.acquireDynamic(dependency)
    dependency.dependants.transform(_ - dependant)
  }

  def handleDiff(dependant: Reactive,newDependencies: Set[Reactive] , oldDependencies: Set[Reactive]): Unit = {
    newDependencies.foreach(turn.acquireDynamic)

    val removedDependencies = oldDependencies.diff(newDependencies)
    removedDependencies.foreach(unregister(dependant))

    val addedDependencies = newDependencies.diff(oldDependencies)
    addedDependencies.foreach(register(dependant))
  }
  
}
