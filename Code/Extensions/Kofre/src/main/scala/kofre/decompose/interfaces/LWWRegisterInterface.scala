package kofre.decompose.interfaces
import kofre.base.DecomposeLattice
import kofre.decompose.*
import kofre.syntax.{PermIdMutate, ArdtOpsContains, OpsSyntaxHelper}
import kofre.contextual.ContextDecompose.DotFun
import kofre.decompose.interfaces.LexCounterInterface.LexCounter
import kofre.decompose.interfaces.MVRegisterInterface.MVRegisterSyntax

/** An LWW (Last Writer Wins) is a Delta CRDT modeling a register.
  *
  * If two concurrent write operations occur, the resulting LWW takes on the value of the write operation with the later timestamp.
  */
object LWWRegisterInterface {
  type LWWRegister[A] = MVRegisterInterface.MVRegister[TimedVal[A]]

  implicit class LWWRegisterSyntax[C, A](container: C)(using ArdtOpsContains[C, LWWRegister[A]]) extends OpsSyntaxHelper[C, LWWRegister[A]](container) {

    def read(using QueryP): Option[A] =
      MVRegisterSyntax(current).read.reduceOption(DecomposeLattice[TimedVal[A]].merge).map(_.value)

    def write(v: A)(using MutationIDP): C =
      MVRegisterSyntax(current).write(TimedVal(v, replicaID))(using PermIdMutate.withID(replicaID))

    def map(f: A => A)(using MutationIDP): C =
      read.map(f) match {
        case None    => DecomposeLattice[LWWRegister[A]].empty
        case Some(v) => write(v)
      }

    def clear()(using MutationIDP): C = MVRegisterSyntax(current).clear()(using PermIdMutate.withID(replicaID))
  }
}
