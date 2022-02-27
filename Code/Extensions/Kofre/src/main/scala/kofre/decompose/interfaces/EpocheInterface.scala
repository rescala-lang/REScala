package kofre.decompose.interfaces

import kofre.decompose.CRDTInterface.{DeltaMutator, DeltaQuery}
import kofre.decompose.UIJDLattice
import kofre.Defs
import kofre.primitives.Epoche

object EpocheInterface {


  def read[E]: DeltaQuery[Epoche[E], E] = {
    case Epoche(_, v) => v
  }

  def mutate[E](m: DeltaMutator[E]): DeltaMutator[Epoche[E]] = {
    case (replicaID, Epoche(c, v)) => Epoche(c, m(replicaID, v))
  }

  def write[E](writeVal: E): DeltaMutator[Epoche[E]] = {
    case (_, Epoche(c, _)) => Epoche(c, writeVal)
  }

  def epocheWrite[E](writeVal: E): DeltaMutator[Epoche[E]] = {
    case (_, Epoche(c, _)) => Epoche(c + 1, writeVal)
  }
}
