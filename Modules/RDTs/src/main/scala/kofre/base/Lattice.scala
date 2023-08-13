package kofre.base

import scala.annotation.targetName
import scala.compiletime.{erasedValue, summonAll, summonFrom, summonInline}
import scala.deriving.Mirror

/** A lattice describes a set of values where we always can [[merge]] two values and get a “consistent” result.
  * Technically, this is a join semilattice. See also [[Bottom]].
  */
@FunctionalInterface
trait Lattice[A] {

  /** By assumption: associative, commutative, idempotent.
    *
    * For use with Delta CRDTs, this function should be optimized for the case
    * that left >> right, i.e., that left is the current state and right the delta
    */
  def merge(left: A, right: A): A

  /** Lattice order is derived from merge, but should be overridden for efficiency */
  def lteq(left: A, right: A): Boolean = merge(left, right) == normalize(right)

  /** Decompose a state into potentially smaller parts.
    * The only requirement is that merging the decomposed results produces the original state.
    * Requires a bottom to enable automatic decomposition of Product types
    * Note that the goal here is small individual storage size at reasonable computational cost.
    * Minimalism of returned results is not guaranteed.
    * It is also not guaranteed that the result does not overlap.
    */
  def decompose(a: A): Iterable[A] = Iterable(a)

  /** Computes delta without state.
    * Overriding this is discouraged.
    */
  def diff(state: A, delta: A): Option[A] = {
    decompose(delta).filter(!lteq(_, state)).reduceOption(merge)
  }

  /** Some types have multiple structural representations for semantically the same value, e.g., they may contain redundant or replaced parts. This can lead to semantically equivalent values that are not structurally equal. Normalize tries to fix this.
    * Overriding this is discouraged.
    */
  def normalize(v: A): A = v merge v

  /** Convenience extensions for the above. */
  /* It would be conceivable to only have the extensions, but the two parameter lists of merge make it not work well with SAM.
   * IntelliJ also does not like to implement or override extension methods. */
  extension (left: A) {
    final inline infix def <=(right: A): Boolean = Lattice.this.lteq(left, right)
    @targetName("mergeInfix")
    final inline infix def merge(right: A): A = Lattice.this.merge(left, right)
    final inline def decomposed: Iterable[A]  = Lattice.this.decompose(left)
  }
}

object Lattice {
  def apply[A](using ev: Lattice[A]): Lattice[A] = ev

  // forwarder for better syntax/type inference
  def merge[A: Lattice](left: A, right: A): A        = apply[A].merge(left, right)
  def lteq[A: Lattice](left: A, right: A): Boolean   = apply[A].lteq(left, right)
  def diff[A: Lattice](left: A, right: A): Option[A] = apply[A].diff(left, right)
  def normalize[A: Lattice](v: A): A                 = apply[A].normalize(v)
  def decompose[A: Lattice](a: A): Iterable[A]       = a.decomposed

  // Sometimes the merge extension on the lattice trait is not found, and it is unclear what needs to be imported.
  // This could be just an extension method, but then would be ambiguous in cases where the extension on the interface is available.
  // Thus, we put the extension into this implicit object, when `Lattice.syntax` is imported (or otherwise in the implicit scope) then it is elegible as the receiver for the extension method rewrite. For some reason, this never causes conflicts even if multiple objects are named `synax` (as opposed to name conflicts with the extension method, which does cause conflicts).
  // In case we ever want to fully migrate away from the  `implicit` keyword, the first line is equivalent to: `given syntax: {} with`, but that seems just weird.
  implicit object syntax:
    extension [A: Lattice](left: A)
      infix def merge(right: A): A = Lattice[A].merge(left, right)

  def latticeOrder[A: Lattice]: PartialOrdering[A] = new {
    override def lteq(x: A, y: A): Boolean = Lattice.lteq(x, y)
    override def tryCompare(x: A, y: A): Option[Int] =
      val lr = lteq(x, y)
      val rl = lteq(y, x)
      (lr, rl) match
        case (true, true)   => Some(0)
        case (false, false) => None
        case (true, false)  => Some(-1)
        case (false, true)  => Some(1)
  }

  def fromOrdering[A: Ordering]: Lattice[A] = new Lattice[A] {
    override def merge(left: A, right: A): A      = if lteq(left, right) then right else left
    override def lteq(left: A, right: A): Boolean = Ordering[A].lteq(left, right)
  }

  // /////////////// common instances below ///////////////

  given setLattice[A]: Lattice[Set[A]] with
    override def merge(left: Set[A], right: Set[A]): Set[A] = left union right
    override def lteq(left: Set[A], right: Set[A]): Boolean = left subsetOf right
    override def decompose(state: Set[A]): Iterable[Set[A]] = state.map(Set(_))

  given optionLattice[A: Lattice]: Lattice[Option[A]] =
    given Lattice[None.type] = Lattice.derived
    given Lattice[Some[A]]   = Lattice.derived
    Lattice.sumLattice

  given mapLattice[K, V: Lattice]: Lattice[Map[K, V]] = new Lattice[Map[K, V]] {
    override def merge(left: Map[K, V], right: Map[K, V]): Map[K, V] =
      val (small, large) =
        // compare unsigned treats the “unknown” value -1 as larger than any known size
        if 0 <= Integer.compareUnsigned(left.knownSize, right.knownSize)
        then (right, left)
        else (left, right)
      small.foldLeft(large) {
        case (current, (key, r)) =>
          current.updatedWith(key) {
            case Some(l) => Some(l merge r)
            case None    => Some(r)
          }
      }

    override def lteq(left: Map[K, V], right: Map[K, V]): Boolean =
      left.forall { (k, l) =>
        right.get(k).exists(r => l <= r)
      }

    override def decompose(state: Map[K, V]): Iterable[Map[K, V]] =
      for
        case (k, v) <- state
        d <- v.decomposed
      yield Map(k -> d)
  }

  given functionLattice[K, V: Lattice]: Lattice[K => V] = (left, right) => k => left(k) merge right(k)

  /** This causes tuple lattices to be generally derivable implicitly,
    * without making all products derivable implicitly.
    */
  inline given tupleLattice[T <: Tuple](using pm: Mirror.ProductOf[T]): Lattice[T] = derived

  inline def sumLattice[T](using sm: Mirror.SumOf[T]): Lattice[T] =
    val lattices: Tuple = summonAll[Tuple.Map[sm.MirroredElemTypes, Lattice]]
    new Derivation.SumLattice[T](sm, lattices)

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): Lattice[T] = {
    val lattices: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, Lattice]]
    val bottoms: Tuple  = Derivation.summonAllMaybe[Tuple.Map[pm.MirroredElemTypes, Bottom]]
    new Derivation.ProductLattice[T](lattices, bottoms, pm, valueOf[pm.MirroredLabel])
  }

  object Derivation {

    class SumLattice[T](sm: Mirror.SumOf[T], lattices: Tuple) extends Lattice[T] {

      private def lat(i: Int): Lattice[T] = lattices.productElement(i).asInstanceOf[Lattice[T]]

      def merge(left: T, right: T): T =
        val lo = sm.ordinal(left)
        val ro = sm.ordinal(right)
        Integer.compare(lo, ro) match
          case 0          => lat(lo).merge(left, right)
          case x if x < 0 => right
          case x if x > 0 => left

      override def lteq(left: T, right: T): Boolean =
        val lo = sm.ordinal(left)
        val ro = sm.ordinal(right)
        Integer.compare(lo, ro) match
          case 0     => lat(lo).lteq(left, right)
          case other => other < 0

      override def decompose(a: T): Iterable[T] = lat(sm.ordinal(a)).decompose(a)
    }

    inline def summonAllMaybe[T <: Tuple]: T =
      val res =
        inline erasedValue[T] match
          case _: EmptyTuple => EmptyTuple
          case _: (τ *: τs) => summonFrom {
              case b: τ => b
              case _    => null
            } *: summonAllMaybe[τs]
        end match
      res.asInstanceOf[T]

    class ProductLattice[T <: Product](
        lattices: Tuple,
        bottoms: Tuple,
        pm: Mirror.ProductOf[T],
        label: String
    ) extends Lattice[T] {

      override def toString: String = s"ProductLattice[${label}]"

      private def lat(i: Int): Lattice[Any] = lattices.productElement(i).asInstanceOf[Lattice[Any]]
      private def bot(i: Int, default: Any): Any =
        val btm = bottoms.productElement(i)
        if btm == null
        then default
        else btm.asInstanceOf[Bottom[Any]].empty
      private def isEmpty(i: Int)(a: Any): Boolean =
        val btm = bottoms.productElement(i)
        if btm == null then false else btm.asInstanceOf[Bottom[Any]].isEmpty(a)

      override def merge(left: T, right: T): T =
        pm.fromProduct(new Product {
          def canEqual(that: Any): Boolean = false
          def productArity: Int            = lattices.productArity
          def productElement(i: Int): Any  = lat(i).merge(left.productElement(i), right.productElement(i))
        })

      override def decompose(a: T): Iterable[T] =
        // Singleton types (product arity == 0) would return an empty iterable if not handled explicitly.
        // That would be fine if we could guarantee that every singleton type has a bottom instance (producing the singleton value), but we currently do not do that
        if lattices.productArity == 0 then Iterable(a)
        else
          Range(0, lattices.productArity).flatMap { j =>
            lat(j).decompose(a.productElement(j)).iterator.filterNot(isEmpty(j)).map { elem =>
              pm.fromProduct(new Product {
                def canEqual(that: Any): Boolean = false
                def productArity: Int            = lattices.productArity
                def productElement(i: Int): Any  = if i == j then elem else bot(i, a.productElement(i))
              })
            }
          }

      override def lteq(left: T, right: T): Boolean = Range(0, lattices.productArity).forall { i =>
        lat(i).lteq(left.productElement(i), right.productElement(i))
      }
    }
  }
}
