package kofre.syntax

import kofre.base.Defs.Id
import kofre.base.Lattice
import kofre.base.Defs

/** The basic idea behind this machinery is to allow lattices of type L to be stored in a Container of type C.
  * In the simplest case C = L and the lattice is used as is.
  * More complex containers contain additional information such as the replica ID
  * or a set of deltas since the last synchronization.
  * No matter the concrete container, they should all offer the same API to the underlying lattice.
  */

trait QueryCtx[C, L]:
  def query(c: C): L
trait MutateCtx[C, L] extends QueryCtx[C, L]:
  def mutate(c: C, delta: L): C
trait IdentifierCtx[C]:
  def replicaId(c: C): Id
class FixedIdCtx[C](id: Id) extends IdentifierCtx[C]:
  override def replicaId(c: C): Id = id
trait AllPermissionsCtx[C, L] extends IdentifierCtx[C], MutateCtx[C, L]

object QueryCtx:
  given identityQuery[A]: QueryCtx[A, A] = MutateCtx.identityDeltaMutate
object MutateCtx:
  given identityDeltaMutate[A]: MutateCtx[A, A] with
    override def query(c: A): A            = c
    override def mutate(c: A, delta: A): A = delta
object AllPermissionsCtx:
  def withID[C, L](id: Id)(using mctx: MutateCtx[C, L]): AllPermissionsCtx[C, L] = new AllPermissionsCtx[C, L]:
    def mutate(c: C, delta: L): C = mctx.mutate(c, delta)
    def replicaId(c: C): Id       = id
    def query(c: C): L            = mctx.query(c)


/** Helper trait to state that container C contains lattices of type L.
  * This is used for better type inference */
trait ArdtOpsContains[C, L]
object ArdtOpsContains:
  given identityContains[L]: ArdtOpsContains[L, L] = new ArdtOpsContains[L, L] {}

/** Helps to define operations that update any container [[C]] containing values of type [[L]]
  * using a scheme where mutations return deltas which are systematically applied.
  */
trait OpsSyntaxHelper[C, L](container: C) {
  final type MutationIDP = AllPermissionsCtx[C, L]
  final type QueryP      = QueryCtx[C, L]
  final type MutationP   = MutateCtx[C, L]
  final type IdentifierP = IdentifierCtx[C]

  final type MutationID = MutationIDP ?=> C
  final type Mutation   = MutationP ?=> C
  final type Query[T]   = QueryP ?=> T

  final protected def current(using perm: QueryP): L                    = perm.query(container)
  final protected def replicaID(using perm: IdentifierP): Defs.Id       = perm.replicaId(container)
  final protected given mutate(using perm: MutationP): Conversion[L, C] = perm.mutate(container, _)
}
