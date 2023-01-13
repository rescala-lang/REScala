package kofre.syntax

import kofre.base.Id
import kofre.base.{DecomposeLattice, Id, Lattice}
import kofre.time.Dots
import kofre.dotted.Dotted

import scala.annotation.implicitNotFound
import scala.util.NotGiven

/** The basic idea behind this machinery is to allow lattices of type L to be stored in a Container of type C.
  * In the simplest case C = L and the lattice is used as is.
  * More complex containers contain additional information such as the replica ID
  * or a set of deltas since the last synchronization.
  * No matter the concrete container, they should all offer the same API to the underlying lattice.
  */

@implicitNotFound("Requires query permission »${L}«\nfrom »${C}")
trait PermQuery[C, L]:
  def query(c: C): L
object PermQuery:
  given identityQuery[A]: PermQuery[A, A] = PermMutate.identityDeltaMutate

@implicitNotFound("Requires mutation permission.\nUnsure to modify »${L}«\nwithin »${C}«")
trait PermMutate[C, L] extends PermQuery[C, L]:
  def mutate(c: C, delta: L): C
object PermMutate:
  given identityDeltaMutate[A]: PermMutate[A, A] with
    override def query(c: A): A            = c
    override def mutate(c: A, delta: A): A = delta

@implicitNotFound(
  "Requires a replica ID.\nWhich seems unavailable in »${C}«\nMissing a container?"
)
trait PermId[C]:
  def replicaId(c: C): Id
class FixedId[C](id: Id) extends PermId[C]:
  override def replicaId(c: C): Id = id

@implicitNotFound(
  "Requires causal context permission.\nNo context in »${C}«\nMissing a container?"
)
trait PermCausal[C]:
  def context(c: C): Dots

@implicitNotFound(
  "Requires context mutation permission.\nUnsure how to extract context from »${C}«\nto modify »${L}«"
)
trait PermCausalMutate[C, L] extends PermCausal[C], PermQuery[C, L]:
  def mutateContext(container: C, withContext: Dotted[L]): C

@implicitNotFound(
  "Requires query, id, and mutation permission.\nUnsure how to extract them from »${C}«\nto modify »${L}«"
)
trait PermIdMutate[C, L] extends PermId[C], PermMutate[C, L]
object PermIdMutate:
  given autoCombin[C, L](using i: PermId[C], m: PermMutate[C, L]): PermIdMutate[C, L] with
    export i.replicaId
    export m.mutate
    export m.query
  given withID[C, L](using id: Id)(using mctx: PermMutate[C, L]): PermIdMutate[C, L] = new PermIdMutate[C, L]:
    def mutate(c: C, delta: L): C = mctx.mutate(c, delta)
    def replicaId(c: C): Id       = id
    def query(c: C): L            = mctx.query(c)

/** Helps to define operations that update any container [[C]] containing values of type [[L]]
  * using a scheme where mutations return deltas which are systematically applied.
  */
trait OpsTypes[C, L] {
  final type MutationIdP     = PermIdMutate[C, L]
  final type QueryP          = PermQuery[C, L]
  final type MutationP       = PermMutate[C, L]
  final type IdentifierP     = PermId[C]
  final type CausalP         = PermCausal[C]
  final type CausalMutationP = PermCausalMutate[C, L]
}
trait OpsSyntaxHelper[C, L](container: C) extends OpsTypes[C, L] {
  final protected[kofre] def current(using perm: QueryP): L                      = perm.query(container)
  final protected[kofre] given replicaID(using perm: IdentifierP): Id            = perm.replicaId(container)
  final protected[kofre] def context(using perm: CausalP): Dots                  = perm.context(container)
  extension (l: L)(using perm: MutationP) def mutator: C                         = perm.mutate(container, l)
  extension (l: Dotted[L])(using perm: CausalMutationP) def mutator: C           = perm.mutateContext(container, l)
  extension [A](c: Dotted[A]) def inheritId(using IdentifierP): Named[Dotted[A]] = c.named(replicaID)
  extension [A](a: A)
    def inheritContext(using CausalP): Dotted[A] = Dotted(a, context)
  extension [A](a: A)
    def inherit(using IdentifierP, CausalP): Named[Dotted[A]] = Dotted(a, context).named(replicaID)

}
