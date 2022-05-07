package kofre.decompose.experimental

import kofre.causality.CausalContext
import kofre.decompose.DecomposableDotStore.{DotMap, DotSet}
import kofre.decompose.interfaces.AWSetInterface
import kofre.dotbased.WithContext

class AWSetDeltaFold[E, B](acc: B, onAdd: (B, E) => B, onRemove: (B, E) => B) {
  type C = CausalContext
  def apply(currentState: AWSetInterface.AWSet[E], deltaState: AWSetInterface.AWSet[E]): AWSetDeltaFold[E, B] =
    deltaState match {
      case WithContext(dm, cc) =>
        val removedDots = cc diff DotMap[E, CausalContext].dots(dm)
        val removedElems = removedDots.iterator.flatMap { dot =>
          currentState.store.collect {
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
