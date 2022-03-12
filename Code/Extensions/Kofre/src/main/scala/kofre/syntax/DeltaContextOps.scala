package kofre.syntax

import kofre.{Defs, Lattice}

trait QueryCtx[C, L]:
  def query(c: C): L
trait MutateCtx[C, L] extends QueryCtx[C, L]:
  def mutate(c: C, delta: L): C
trait IdentifierCtx[C]:
  def replicaId(c: C): Defs.Id
trait AllPermissionsCtx[C, L] extends IdentifierCtx[C], MutateCtx[C, L]

object QueryCtx:
  given identityQuery[A]: QueryCtx[A, A] with
    override def query(c: A): A = c

/** Helps to define operations that update any container [[C]] containing values of type [[L]]
  * using a scheme where mutations return deltas which are systematically applied.
  */
trait OpsSyntaxHelper[C, L](c: C) {
  final type MutationIDP = AllPermissionsCtx[C, L]
  final type QueryP      = QueryCtx[C, L]
  final type MutationP   = MutateCtx[C, L]
  final type MutationID  = MutationIDP ?=> C
  final type Mutation    = MutationP ?=> C
  final type Query[T]    = QueryP ?=> T
  final def current(using perm: QueryCtx[C, L]): L                  = perm.query(c)
  final def replicaID(using perm: IdentifierCtx[C]): Defs.Id        = perm.replicaId(c)
  final given mutate(using perm: MutateCtx[C, L]): Conversion[L, C] = perm.mutate(c, _)
}
