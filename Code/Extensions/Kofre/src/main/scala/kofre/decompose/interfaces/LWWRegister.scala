package kofre.decompose.interfaces
import kofre.base.DecomposeLattice
import kofre.datatypes.TimedVal
import kofre.decompose.*
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate}
import kofre.decompose.interfaces.MVRegister.syntax
import kofre.dotted.{DotFun, Dotted, DottedDecompose, DottedLattice}

/** An LWW (Last Writer Wins) is a Delta CRDT modeling a register.
  *
  * If two concurrent write operations occur, the resulting LWW takes on the value of the write operation with the later timestamp.
  */
case class LWWRegister[A](repr: MVRegister[TimedVal[A]])

object LWWRegister {
  def empty[A]: LWWRegister[A] = LWWRegister(MVRegister.empty)

  given dottedDecompose[A]: DottedDecompose[LWWRegister[A]] = DottedDecompose.derived

  implicit class LWWRegisterSyntax[C, A](container: C)(using ArdtOpsContains[C, LWWRegister[A]])
      extends OpsSyntaxHelper[C, LWWRegister[A]](container) {

    def read(using QueryP): Option[A] =
      current.repr.read.reduceOption(DecomposeLattice[TimedVal[A]].merge).map(x => x.value)

    def write(v: A)(using CausalMutationP, IdentifierP): C =
      MVRegister.syntax(context.wrap(current.repr).named(replicaID)).write(TimedVal(v, replicaID)).anon.map(
        LWWRegister.apply
      ).mutator

    def map(f: A => A)(using CausalMutationP, IdentifierP): C =
      read.map(f) match {
        case None    => Dotted(LWWRegister.empty).mutator
        case Some(v) => write(v)
      }

    def clear()(using CausalMutationP): C = context.wrap(current.repr).clear()(using
    Dotted.syntaxPermissions(using MVRegister.dottedDecompose)).map(LWWRegister.apply).mutator
  }
}
