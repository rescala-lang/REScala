package kofre.decompose.experimental

import kofre.causality.CausalContext
import kofre.contextual.ContextDecompose.{DotMap, DotSet}
import kofre.contextual.WithContext
import kofre.predef.AddWinsSet

class AWSetDeltaFold[E, B](acc: B, onAdd: (B, E) => B, onRemove: (B, E) => B) {
  type C = CausalContext
  def apply(currentState: AddWinsSet[E], deltaState: AddWinsSet[E]): AWSetDeltaFold[E, B] =
    deltaState match {
      case AddWinsSet(WithContext(dm, cc)) =>
        val removedDots = cc diff DotMap[E, CausalContext].dots(dm)
        val removedElems = removedDots.iterator.flatMap { dot =>
          currentState.inner.store.collect {
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
