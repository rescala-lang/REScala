package kofre.dotted

import kofre.base.{Bottom, Lattice}
import kofre.time.Dots

import scala.annotation.targetName
import scala.compiletime.{erasedValue, summonAll, summonInline}
import scala.deriving.Mirror

/** The delta CRDT paper introduces a lot of individual abstractions
  * that all do a lot of cool stuff, but often not separated into their pieces.
  * This is one of those pieces systematically handling removals based on metadata.
  *
  * The context encodes dots that have been seen.
  * The store of type [[A]] is assumed to be something that stores individual information per dot.
  * We represent removals as context that contains a dot, but a store that no longer contains the dot.
  * Thus, when merging the abstract logic is somewhat along the lines of:
  *   Both contain the (dot, value) pair? => merge value recursively.
  *   Only one contains the dot in the context? => keep that value.
  *   Both contain the dot, but at least one has no matching value? => remove all values for that dot.
  *
  * Separating into a [[mergePartial]] allows extracting the context into the outermost layer reducing metadata overhead.
  */
trait DottedLattice[A] extends Lattice[Dotted[A]] {

  /** Partial merging combines the stored values, but ignores the context.
    * Thus enabling nested merging of values, without merging context multiple times.
    */
  def mergePartial(left: Dotted[A], right: Dotted[A]): A

  def merge(left: Dotted[A], right: Dotted[A]): Dotted[A] =
    Dotted(
      mergePartial(left, right),
      left.context.merge(right.context)
    )

  override def lteq(left: Dotted[A], right: Dotted[A]): Boolean =
    if !(left.context <= right.context) then false
    else super.lteq(left, right)

  extension (left: Dotted[A])
    @targetName("mergePartialExtension")
    def mergePartial(right: Dotted[A]): A = this.mergePartial(left, right)

  def contextbimap[B](to: Dotted[A] => Dotted[B], from: Dotted[B] => Dotted[A]): DottedLattice[B] =
    new DottedLattice[B] {
      override def lteq(left: Dotted[B], right: Dotted[B]): Boolean = DottedLattice.this.lteq(from(left), from(right))
      override def decompose(a: Dotted[B]): Iterable[Dotted[B]]     = DottedLattice.this.decompose(from(a)).map(to)
      override def mergePartial(left: Dotted[B], right: Dotted[B]): B =
        to(Dotted(DottedLattice.this.mergePartial(from(left), from(right)))).data
    }
}

object DottedLattice {

  case class Partitioned[A](keep: A, remove: A)

  def apply[A: DottedLattice]: DottedLattice[A] = summon

  def decomposedDeletions[A: HasDots: Bottom](dotted: Dotted[A]) =
    dotted.deletions.decomposed.map(d => Dotted(Bottom.empty, d))

  given optionInstance[A: DottedLattice: HasDots]: DottedLattice[Option[A]] with {
    override def mergePartial(left: Dotted[Option[A]], right: Dotted[Option[A]]): Option[A] =
      (left.data, right.data) match
        case (None, None)       => None
        case (None, Some(r))    => r.removeDots(left.context)
        case (Some(l), None)    => l.removeDots(right.context)
        case (Some(l), Some(r)) => Some(DottedLattice.apply.mergePartial(left.map(_ => l), right.map(_ => r)))

    override def lteq(left: Dotted[Option[A]], right: Dotted[Option[A]]): Boolean =
      (left.context <= right.context) &&
      ((left.data, right.data) match
        case (None, _)          => true
        case (_, None)          => false
        case (Some(l), Some(r)) => left.map(_ => l) <= right.map(_ => r)
      )
  }

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): DottedLattice[T] = {
    val lattices: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, DottedLattice]]
    val bottoms: Tuple  = Lattice.Derivation.summonAllMaybe[Tuple.Map[pm.MirroredElemTypes, Bottom]]
    val hasDots: Tuple  = Lattice.Derivation.summonAllMaybe[Tuple.Map[pm.MirroredElemTypes, HasDots]]
    new ProductDottedLattice[T](lattices, bottoms, hasDots, pm, valueOf[pm.MirroredLabel])
  }

  class ProductDottedLattice[T <: Product](
      lattices: Tuple,
      bottoms: Tuple,
      hasDots: Tuple,
      pm: Mirror.ProductOf[T],
      label: String
  ) extends DottedLattice[T] {

    override def toString: String = s"DottedProductLattice[${label}]"

    private def lat(i: Int): DottedLattice[Any] = lattices.productElement(i).asInstanceOf[DottedLattice[Any]]
    private def bot(i: Int): Bottom[Any]        = bottoms.productElement(i).asInstanceOf[Bottom[Any]]
    private def hdots(i: Int): HasDots[Any]     = hasDots.productElement(i).asInstanceOf[HasDots[Any]]
    private lazy val dotsAndBottoms =
      bottoms.productIterator.forall(v => null != v) &&
      hasDots.productIterator.forall(v => null != v)

    override def mergePartial(left: Dotted[T], right: Dotted[T]): T =
      pm.fromProduct(new Product {
        def canEqual(that: Any): Boolean = false
        def productArity: Int            = lattices.productArity
        def productElement(i: Int): Any =
          lat(i).mergePartial(left.map(_.productElement(i)), right.map(_.productElement(i)))
      })

    override def decompose(a: Dotted[T]): Iterable[Dotted[T]] =
      if !dotsAndBottoms then Iterable(a)
      else
        val bottomValues = Range(0, bottoms.productArity).map(i => bot(i).empty)
        // to compute the added elements, we decompose each component individually, using a context that only includes the contained dots, and fill the other components with bottom values
        val added: List[Dotted[T]] =
          Range(0, lattices.productArity).iterator.flatMap: index =>
            val element = a.data.productElement(index)
            val dots    = hdots(index).dots(element)
            lat(index).decompose(Dotted(element, dots)).map:
              case Dotted(decomposedElement, decomposedContext) =>
                Dotted(
                  pm.fromProduct(new Product {
                    def canEqual(that: Any): Boolean = false
                    def productArity: Int            = lattices.productArity
                    def productElement(i: Int): Any = if i == index then decomposedElement
                    else bottomValues(i)
                  }),
                  decomposedContext
                )
          .toList

        // This takes the list of individual deltas above, and re-merges all the ones that have an overlapping context because those may not be split into multiple deltas, otherwise they would remove each other on merge
        def compact(rem: List[Dotted[T]], acc: List[Dotted[T]]): List[Dotted[T]] = rem match
          case Nil => acc
          case h :: t =>
            def overlap(e: Dotted[T]) = !h.context.disjunct(e.context)
            val (tin, tother)         = t.span(overlap)
            val (accin, accother)     = acc.span(overlap)
            val all                   = tin ++ accin
            val compacted = all.foldLeft(h): (l, r) =>
              Dotted(mergePartial(Dotted(l.data, Dots.empty), Dotted(r.data, Dots.empty)), l.context union r.context)

            compact(tother, compacted :: accother)
        val compacted = compact(added, Nil)

        val containedDots = compacted.map(_.context).reduceOption(_ merge _).getOrElse(Dots.empty)
        val removed       = a.context.diff(containedDots)
        def empty = pm.fromProduct(new Product {
          def canEqual(that: Any): Boolean = false

          def productArity: Int = lattices.productArity

          def productElement(i: Int): Any = bottomValues(i)
        })
        if removed.isEmpty
        then compacted
        else compacted :+ Dotted(empty, removed)

    override def lteq(left: Dotted[T], right: Dotted[T]): Boolean =
      Range(0, lattices.productArity).forall { i =>
        lat(i).lteq(left.map(_.productElement(i)), right.map(_.productElement(i)))
      }
  }

  /** Enables the use of a [[kofre.base.Lattice]] as a [[DottedLattice]].
    * Beware that this works for most datastructures due to automatic derivation of the required instance,
    * but will likely not have the intended semantics if the datastructure does use any dots inside.
    */
  def liftLattice[A: Lattice]: DottedLattice[A] =
    new DottedLattice[A] {
      override def mergePartial(left: Dotted[A], right: Dotted[A]): A =
        Lattice[A].merge(left.data, right.data)

      override def lteq(left: Dotted[A], right: Dotted[A]): Boolean =
        Lattice[A].lteq(left.data, right.data)

      override def decompose(state: Dotted[A]): Iterable[Dotted[A]] = {
        Lattice[A].decompose(state.data).map(Dotted(_, Dots.empty))
      }
    }
}
