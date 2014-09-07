package rescala.signals

import rescala.IFunctions
import rescala.events.Event

class SwitchedSignal[+T, -E](trigger: Event[E], initialSignal: Signal[T], initialFactory: IFunctions.Factory[E, T])
  extends DependentSignalImplementation[T] {

  val fold = trigger.fold((initialSignal, initialFactory)) { case ((_, factory), pulse) =>  factory(pulse) }

  setDependOn(Set(fold, initialSignal))

  override def initialValue(): T = initialSignal.get

  override def calculateNewValue(): T = {
    setDependOn(Set(fold, fold.get._1))
    fold.get._1.get
  }
}
