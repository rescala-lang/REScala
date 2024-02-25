package lore.dsl

trait Interaction[S, A] {
  type T[_ <: S, _ <: A] <: Interaction[S, A]

  inline def requires(inline pred: (S, A) => Boolean): T[S, A]

  inline def ensures(inline pred: (S, A) => Boolean): T[S, A]

}

object Interaction {
  inline def apply[S, A](using invariantManager: InvariantManager): UnboundInteraction[S, A] =
    UnboundInteraction(Seq.empty, Seq.empty, invariantManager)

}

trait CanExecute[S, A] {
  type E[_ <: S, _ <: A] <: Interaction[S, A]

  inline def executes(inline fun: (S, A) => S): E[S, A]
}

trait CanModify[S, A] {
  type M[_ <: S, _ <: A] <: Interaction[S, A]

  inline def modifies(inline expr: Source[S]): M[S, A]
}

trait CanApply[S, A] {
  def apply(arg: A): Unit
}
