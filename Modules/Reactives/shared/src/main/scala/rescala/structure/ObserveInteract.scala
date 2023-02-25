package rescala.structure

import rescala.core.Observation

trait ObserveInteract extends Observation {
  // if true, the observer will remove all of its inputs, which allows eventual collection
  def checkExceptionAndRemoval(): Boolean
}
