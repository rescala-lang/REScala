package kofre.decompose.interfaces

import kofre.Defs
import kofre.decompose.UIJDLattice
import kofre.decompose.interfaces.GCounterInterface.GCounter
import kofre.primitives.Epoche
import kofre.syntax.AllPermissionsCtx.withID
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper}

object EpocheInterface {

  implicit class EpocheSyntax[C, E](container: C)(using ArdtOpsContains[C, Epoche[E]]) extends OpsSyntaxHelper[C, Epoche[E]](container) {
    def read(using QueryP): E = current.value

    def write(value: E)(using MutationP): C       = current.copy(value = value)
    def epocheWrite(value: E)(using MutationP): C = Epoche(current.counter + 1, value)

    def map(f: E => E)(using MutationP): C = write(f(current.value))
  }
}
