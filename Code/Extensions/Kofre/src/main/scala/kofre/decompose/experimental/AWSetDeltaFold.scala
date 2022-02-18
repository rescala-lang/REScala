package kofre.decompose.experimental

import kofre.causality.CausalContext
import kofre.decompose.DotStore.{DotMap, DotSet}
import kofre.decompose.interfaces.AWSetInterface
import kofre.dotbased.CausalStore

class AWSetDeltaFold[E, B](acc: B, onAdd: (B, E) => B, onRemove: (B, E) => B) {
  type C = CausalContext
  def apply(currentState: AWSetInterface.State[E], deltaState: AWSetInterface.State[E]): AWSetDeltaFold[E, B] =
    deltaState match {
      case CausalStore(dm, cc) =>
        val removedDots = cc.toSet diff DotMap[E, DotSet].dots(dm)
        val removedElems = removedDots.flatMap { dot =>
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
