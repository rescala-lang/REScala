package kofre.dotted

import kofre.base.Lattice.{Operators, optionLattice}
import kofre.base.{Bottom, Lattice}
import kofre.datatypes.ReplicatedList
import kofre.time.{Dot, Dots}

import scala.annotation.{implicitNotFound, targetName}
import scala.compiletime.{erasedValue, summonAll, summonInline}
import scala.deriving.Mirror

type DottedDecompose[A] = DottedLattice[A]
val DottedDecompose: DottedLattice.type = DottedLattice

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
        to(Dotted(DottedLattice.this.mergePartial(from(left), from(right)))).store
    }
}

object DottedLattice {

  def apply[A: DottedLattice]: DottedLattice[A] = summon

  def decomposedDeletions[A: HasDots: Bottom](dotted: Dotted[A]) =
    dotted.deletions.decomposed.map(d => Dotted(Bottom.empty, d))

  given optionInstance[A: DottedLattice: Bottom]: DottedLattice[Option[A]] with {
    override def mergePartial(left: Dotted[Option[A]], right: Dotted[Option[A]]): Option[A] =
      lazy val empty = Bottom.empty
      val res        = left.map(_.getOrElse(empty)) mergePartial right.map(_.getOrElse(empty))
      if res == empty then None else Some(res)

    override def lteq(left: Dotted[Option[A]], right: Dotted[Option[A]]): Boolean =
      (left.context <= right.context) &&
      ((left.store, right.store) match
        case (None, _)          => true
        case (_, None)          => false
        case (Some(l), Some(r)) => left.map(_ => l) <= right.map(_ => r)
      )
  }

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): DottedLattice[T] = {
    val lattices: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, DottedLattice]]
//    val bottoms: Tuple  = summonAll[Tuple.Map[pm.MirroredElemTypes, Bottom]]
    new ProductDottedLattice[T](lattices, null, pm, valueOf[pm.MirroredLabel])
  }

  class ProductDottedLattice[T <: Product](lattices: Tuple, bottoms: Tuple, pm: Mirror.ProductOf[T], label: String)
      extends DottedLattice[T] {

    override def toString: String = s"ProductLattice[${label}]"

    private def lat(i: Int): DottedLattice[Any] = lattices.productElement(i).asInstanceOf[DottedLattice[Any]]
    private def bot(i: Int): Bottom[Any]        = bottoms.productElement(i).asInstanceOf[Bottom[Any]]

    override def mergePartial(left: Dotted[T], right: Dotted[T]): T =
      pm.fromProduct(new Product {
        def canEqual(that: Any): Boolean = false
        def productArity: Int            = lattices.productArity
        def productElement(i: Int): Any =
          lat(i).mergePartial(left.map(_.productElement(i)), right.map(_.productElement(i)))
      })

    // // TODO: figure out what decompose means for dotted values
    // override def decompose(a: Dotted[T]): Iterable[Dotted[T]] =
    // Range(0, lattices.productArity).flatMap { j =>
    //  lat(j).decompose(a.map(_.productElement(j))).map {
    //    _.map { elem =>
    //      pm.fromProduct(new Product {
    //        def canEqual(that: Any): Boolean = false
    //        def productArity: Int            = lattices.productArity
    //        def productElement(i: Int): Any  = if i == j then elem else bot(i).empty
    //      })
    //    }
    //  }
    // }

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
        Lattice[A].merge(left.store, right.store)

      override def lteq(left: Dotted[A], right: Dotted[A]): Boolean =
        Lattice[A].lteq(left.store, right.store)

      override def decompose(state: Dotted[A]): Iterable[Dotted[A]] = {
        Lattice[A].decompose(state.store).map(Dotted(_, Dots.empty))
      }
    }
}
