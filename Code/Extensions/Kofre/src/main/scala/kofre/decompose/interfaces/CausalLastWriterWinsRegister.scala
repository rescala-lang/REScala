package kofre.decompose.interfaces
import kofre.base.{Bottom, DecomposeLattice}
import kofre.datatypes.TimedVal
import kofre.decompose.*
import kofre.syntax.{ArdtOpsContains, OpsSyntaxHelper, PermIdMutate}
import kofre.decompose.interfaces.MultiVersionRegister.syntax
import kofre.dotted.{DotFun, Dotted, DottedDecompose, DottedLattice}

/** An LWW (Last Writer Wins) is a Delta CRDT modeling a register.
  *
  * If two concurrent write operations occur, the resulting LWW takes on the value of the write operation with the later timestamp.
  */
case class CausalLastWriterWinsRegister[A](repr: MultiVersionRegister[TimedVal[A]])

object CausalLastWriterWinsRegister {
  def empty[A]: CausalLastWriterWinsRegister[A] = CausalLastWriterWinsRegister(MultiVersionRegister.empty)

  given bottomInstance[A]: Bottom[CausalLastWriterWinsRegister[A]]           = Bottom.derived
  given dottedDecompose[A]: DottedDecompose[CausalLastWriterWinsRegister[A]] = DottedDecompose.derived

  implicit class syntax[C, A](container: C)(using ArdtOpsContains[C, CausalLastWriterWinsRegister[A]])
      extends OpsSyntaxHelper[C, CausalLastWriterWinsRegister[A]](container) {

    def read(using QueryP): Option[A] =
      current.repr.read.reduceOption(DecomposeLattice[TimedVal[A]].merge).map(x => x.value)

    def write(v: A)(using CausalMutationP, IdentifierP): C =
      MultiVersionRegister.syntax(context.wrap(current.repr).named(replicaID)).write(TimedVal(v, replicaID)).anon.map(
        CausalLastWriterWinsRegister.apply
      ).mutator

    def map(f: A => A)(using CausalMutationP, IdentifierP): C =
      read.map(f) match {
        case None    => Dotted(CausalLastWriterWinsRegister.empty).mutator
        case Some(v) => write(v)
      }

    def clear()(using CausalMutationP): C = context.wrap(current.repr).clear()(using
    Dotted.syntaxPermissions(using MultiVersionRegister.dottedDecompose)).map(
      CausalLastWriterWinsRegister.apply
    ).mutator
  }
}
