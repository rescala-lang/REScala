package rescala.signals

import rescala.IFunctions
import rescala.events.Event
import rescala.propagation.Turn

class SwitchedSignal[+T, -E]
    (trigger: Event[E], initialSignal: Signal[T], initialFactory: IFunctions.Factory[E, T])
    (creationTurn: Turn)
  extends DependentSignalImplementation[T](creationTurn) {

  val fold = trigger.fold((initialSignal, initialFactory)) { case ((_, factory), pulse) =>  factory(pulse) }

  setDependencies(Set(fold, initialSignal))

  override def initialValue()(implicit turn: Turn): T = initialSignal.get

  override def calculateNewValue()(implicit turn: Turn): T = {
    setDependencies(Set(fold, fold.get._1))
    fold.get._1.get
  }
}
