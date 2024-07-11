package rdts.dotted

import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.time.{Dot, Dots}

case class Obrem[A](data: A, observed: Dots, deletions: Dots) {

  def context: Dots = observed `union` deletions

  inline def mod[B](f: Dots ?=> A => Obrem[B]): Obrem[B] = f(using context)(data)
  inline def modn[B](f: A => B): Dotted[B]               = Dotted(f(data))

  /** For temporary compat */
  def toDotted: Dotted[A] = Dotted(data, observed `union` deletions)
}

object Obrem {

  def apply[A: HasDots](data: A): Obrem[A] = Obrem(data, data.dots, Dots.empty)

  def empty[A: Bottom]: Obrem[A] = Obrem(Bottom.empty[A], Dots.empty, Dots.empty)

  given lattice[A: HasDots: Bottom: Lattice]: Lattice[Obrem[A]] with {
    def merge(left: Obrem[A], right: Obrem[A]): Obrem[A] =
      val l =
        if right.deletions.isEmpty then left.data else left.data.removeDots(right.deletions).getOrElse(Bottom.empty)
      val r =
        if left.deletions.isEmpty then right.data else right.data.removeDots(left.deletions).getOrElse(Bottom.empty)
      Obrem(l `merge` r, left.observed `union` right.observed, left.deletions `union` right.deletions)
  }
}

/** Associates a context of Dots with some data structure.
  * The most common use is to interpret the context as the set of:
  * • all dots that are present in data
  * • all dots directly subsumed by data
  *
  * Specifically, the `deletions` and `contained` methods reflect this interpretation.
  */
case class Dotted[A](data: A, context: Dots) {
  def map[B](f: A => B): Dotted[B]      = Dotted(f(data), context)
  def knows(dot: Dot): Boolean          = context.contains(dot)
  def deletions(using HasDots[A]): Dots = context `diff` contained
  def contained(using HasDots[A]): Dots = data.dots
  def advanced(r: LocalUid): Dotted[A]  = Dotted(data, context.advanced(r.uid))

  inline def mod[B](f: A => Dots ?=> Dotted[B]): Dotted[B] = f(data)(using context)
  inline def modn[B](f: A => B): Dotted[B]                 = Dotted(f(data))

  def toObrem(using HasDots[A]) =
    val dots = data.dots
    Obrem(data, dots, context `subtract` dots)
}

type DottedLattice[T] = Lattice[Dotted[T]]
object DottedLattice {
  export Lattice.{apply as _, *}

  def apply[A](using ev: Lattice[Dotted[A]]): Lattice[Dotted[A]] = ev
}

object Dotted {

  def empty[A: Bottom]: Dotted[A] = Dotted(Bottom.empty[A], Dots.empty)
  def apply[A](a: A): Dotted[A]   = Dotted(a, Dots.empty)

  given lattice[A: HasDots: Bottom: Lattice]: Lattice[Dotted[A]] with {
    def merge(left: Dotted[A], right: Dotted[A]): Dotted[A] =
      val l = left.data.removeDots(right.deletions).getOrElse(Bottom.empty)
      val r = right.data.removeDots(left.deletions).getOrElse(Bottom.empty)
      Dotted(l `merge` r, left.context `union` right.context)

    /** Dotted decompose guarantees decomposes its inner value, but recomposes any values with overlapping dots, such that each dot is present in exactly one of the returned deltas. */
    override def decompose(a: Dotted[A]): Iterable[Dotted[A]] =
      val deltas = a.data.decomposed.flatMap: delta =>
        val dots = delta.dots
        Option.when(!dots.isEmpty || !delta.isEmpty):
          Dotted(delta, delta.dots)

      val compacted   = compact(deltas.toList, Nil)
      val presentDots = compacted.iterator.map(_.context).foldLeft(Dots.empty)(_ `union` _)
      assert(a.context.contains(presentDots), "fantasized new dots, this likely means a bug elsewhere")
      val removed = a.context `subtract` presentDots
      val empty   = Bottom[A].empty
      compacted concat removed.decomposed.flatMap(dots => Option.when(!dots.isEmpty)(Dotted(empty, dots)))

  }

  // combines entries with overlapping dots
  def compact[T](rem: List[Dotted[T]], acc: List[Dotted[T]])(using dl: Lattice[T]): List[Dotted[T]] = rem match
    case Nil => acc
    case h :: tail =>
      def overlap(e: Dotted[T]): Boolean = !h.context.disjunct(e.context)

      val (tin, tother)     = tail.partition(overlap)
      val (accin, accother) = acc.partition(overlap)
      val all               = tin ++ accin
      val compacted = all.foldLeft(h): (l, r) =>
        Dotted(dl.merge(l.data, r.data), l.context `union` r.context)

      // have to repeat the check with compacted, until it did not grow
      if all.isEmpty
      then compact(tother, compacted :: accother)
      else compact(compacted :: tother, accother)

  def liftLattice[A: Lattice]: Lattice[Dotted[A]] = new {
    def merge(left: Dotted[A], right: Dotted[A]): Dotted[A] =
      Dotted(left.data `merge` right.data, left.context `union` right.context)
  }

}
