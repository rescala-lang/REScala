package kofre.decompose.experimental

import kofre.decompose.{CContext, Causal}
import kofre.decompose.DotStore.{DotMap, DotSet}
import kofre.decompose.interfaces.AWSetInterface

class AWSetDeltaFold[E, C: CContext, B](acc: B, onAdd: (B, E) => B, onRemove: (B, E) => B) {
  def apply(currentState: AWSetInterface.State[E, C], deltaState: AWSetInterface.State[E, C]): AWSetDeltaFold[E, C, B] =
    deltaState match {
      case Causal(dm, cc) =>
        val removedDots = CContext[C].toSet(cc) diff DotMap[E, DotSet].dots(dm)
        val removedElems = removedDots.flatMap { dot =>
          currentState.dotStore.collect {
            case (e, ds) if ds.contains(dot) => e
          }
        }

        val addedElems = dm.keys

        val afterRemove = removedElems.foldLeft(acc)(onRemove)

        val afterAdd = addedElems.foldLeft(afterRemove)(onAdd)

        new AWSetDeltaFold(afterAdd, onAdd, onRemove)
    }

  def get: B = acc
}
