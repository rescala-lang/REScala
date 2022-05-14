package kofre.decompose.interfaces
import kofre.base.DecomposeLattice
import kofre.decompose.*
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate}
import kofre.contextual.WithContext
import kofre.decompose.interfaces.LexCounterInterface.LexCounter
import kofre.decompose.interfaces.MVRegisterInterface.MVRegisterSyntax

/** An LWW (Last Writer Wins) is a Delta CRDT modeling a register.
  *
  * If two concurrent write operations occur, the resulting LWW takes on the value of the write operation with the later timestamp.
  */
object LWWRegisterInterface {
  type LWWRegister[A] = MVRegisterInterface.MVRegister[TimedVal[A]]
  def empty[A]: LWWRegister[A] = Map.empty

  implicit class LWWRegisterSyntax[C, A](container: C)(using ArdtOpsContains[C, LWWRegister[A]]) extends OpsSyntaxHelper[C, LWWRegister[A]](container) {

    def read(using QueryP): Option[A] =
      MVRegisterSyntax(current).read.reduceOption(DecomposeLattice[TimedVal[A]].merge).map(_.value)

    def write(v: A)(using CausalMutationP, IdentifierP): C =
      MVRegisterSyntax(context.wrap(current).named(replicaID)).write(TimedVal(v, replicaID)).anon.mutator

    def map(f: A => A)(using CausalMutationP, IdentifierP): C =
      read.map(f) match {
        case None    => WithContext(DecomposeLattice[LWWRegister[A]].empty).mutator
        case Some(v) => write(v)
      }

    def clear()(using CausalMutationP): C = context.wrap(current).clear().mutator
  }
}
