package rdts.base

import scala.annotation.targetName
import scala.collection.IterableOps
import scala.collection.immutable.MapOps
import scala.compiletime.{erasedValue, summonAll, summonFrom, summonInline}
import scala.deriving.Mirror

/** A lattice describes a set of values where we always can [[merge]] two values and get a “consistent” result.
  * Technically, this is a join semilattice. See also [[Bottom]].
  */
@FunctionalInterface
trait Lattice[A] {

  /** By assumption: associative, commutative, idempotent.
    *
    * Implementation note: If it matters, assume that `left` is the current state and `right` is an added delta.
    * All code should assume that `left` is the larger state (and optimize for this).
    * If `left == right`, prefer to return `left`.
    */
  def merge(left: A, right: A): A

  /** Lattice order is derived from merge.
    * Overriding implementations must make sure that they compute exactly the same results as the equation below.
    */
  def subsumption(left: A, right: A): Boolean = merge(left, right) == Lattice.normalize(right)(using this)

  /** Convenience extensions for the above. */
  /* It would be conceivable to only have the extensions, but the two parameter lists of merge make it not work well with SAM.
   * IntelliJ also does not like to implement or override extension methods. */
  extension (left: A) {

    /** Merging `right` into `left` has no effect */
    inline def subsumes(right: A): Boolean = Lattice.this.subsumption(right, left)

    /** Merging `left` and `right` would be strictly larger than right */
    inline def inflates(right: A): Boolean = !Lattice.this.subsumption(left, right)

    @targetName("mergeInfix")
    inline def merge(right: A): A = Lattice.this.merge(left, right)
  }
}

object Lattice {
  def apply[A](using ev: Lattice[A]): Lattice[A] = ev

  // forwarder for better syntax/type inference
  def merge[A: Lattice as A](left: A, right: A): A = A.merge(left, right)

  /** Some types have multiple structural representations for semantically the same value, e.g., they may contain redundant or replaced parts. This can lead to semantically equivalent values that are not structurally equal. Normalize tries to fix this. */
  def normalize[A: Lattice](v: A): A = v `merge` v

  def diff[A: {Lattice as A, Decompose}](state: A, delta: A): Option[A] = {
    delta.decomposed.filter(!A.subsumption(_, state)).reduceOption(merge)
  }

  // Sometimes the merge extension on the lattice trait is not found, and it is unclear what needs to be imported.
  // This could be just an extension method, but then would be ambiguous in cases where the extension on the interface is available.
  // Thus, we put the extension into this implicit object, when `Lattice.syntax` is imported (or otherwise in the implicit scope) then it is eligible as the receiver for the extension method rewrite. For some reason, this never causes conflicts even if multiple objects are named `syntax` (as opposed to name conflicts with the extension method, which does cause conflicts).
  // also, intellij does find these, but not the ones on the trait … ?
  given syntax: {} with
    extension [A: Lattice as A](left: A) {
      def merge(right: A): A = A.merge(left, right)

      /** Convenience method to apply delta mutation to grow current value */
      def grow(f: A => A): A = A.merge(left, f(left))

      inline def inflates(right: A): Boolean = !A.subsumption(left, right)
      inline def subsumes(right: A): Boolean = A.subsumption(right, left)
    }

  def latticeOrder[A: Lattice as A]: PartialOrdering[A] = new {
    override def lteq(x: A, y: A): Boolean = A.subsumption(x, y)
    override def tryCompare(x: A, y: A): Option[Int] =
      val lr = lteq(x, y)
      val rl = lteq(y, x)
      (lr, rl) match
        case (true, true)   => Some(0)
        case (false, false) => None
        case (true, false)  => Some(-1)
        case (false, true)  => Some(1)

    // overrides because parent implementation calls lteq(x, y) twice
    override def lt(x: A, y: A): Boolean = lteq(x, y) && !lteq(y, x)
    override def gt(x: A, y: A): Boolean = gteq(x, y) && !lteq(x, y)
  }

  def fromOrdering[A: Ordering]: Lattice[A] = new Lattice[A] {
    override def merge(left: A, right: A): A             = if subsumption(left, right) then right else left
    override def subsumption(left: A, right: A): Boolean = Ordering[A].lteq(left, right)
  }

  def assertEquals[A]: Lattice[A] = (left: A, right: A) =>
    if left == right then left
    else throw new IllegalStateException(s"assumed there would be no conflict, but have $left and $right")

  def assertEqualsOrdering[A]: Ordering[A] = (l, r) =>
    if l == r then 0
    else throw IllegalStateException(s"assumed equality does not hold for »$l« and »$r« ")

  // /////////////// common instances below ///////////////

  given setLattice[A]: Lattice[Set[A]] with
    override def merge(left: Set[A], right: Set[A]): Set[A]        = left `union` right
    override def subsumption(left: Set[A], right: Set[A]): Boolean = left subsetOf right

  given optionLattice[A: Lattice]: Lattice[Option[A]] =
    given Lattice[None.type] = Lattice.derived
    given Lattice[Some[A]]   = Lattice.derived
    Lattice.sumLattice

  given mapLattice[K, V: Lattice, Mp[K1, +V1] <: MapOps[K1, V1, Mp, Mp[K1, V1]]]: Lattice[Mp[K, V]] =
    new Lattice[Mp[K, V]] {
      override def merge(left: Mp[K, V], right: Mp[K, V]): Mp[K, V] =
        val (small: Mp[K, V], large: Mp[K, V]) =
          // compare unsigned treats the “unknown” value -1 as larger than any known size
          if 0 <= Integer.compareUnsigned(left.knownSize, right.knownSize)
          then (right, left)
          else (left, right)
        small.foldLeft(large) {
          case (current, (key, r)) =>
            current.updatedWith(key) {
              case Some(l) => Some(l `merge` r)
              case None    => Some(r)
            }
        }

      override def subsumption(left: Mp[K, V], right: Mp[K, V]): Boolean =
        left.forall { (k, l) =>
          right.get(k).exists(r => r `subsumes` l)
        }

    }

  given iterableLattice[A, It[B] <: IterableOps[B, It, It[B]]](using Lattice[A]): Lattice[It[A]] = (left, right) => {
    val li  = left.iterator
    val ri  = right.iterator
    val res = li.zip(ri).map(Lattice.merge) ++ li ++ ri

    res.to(left.iterableFactory.iterableFactory)
  }

  given functionLattice[K, V: Lattice]: Lattice[K => V] = (left, right) => k => left(k) `merge` right(k)

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

      override def subsumption(left: T, right: T): Boolean =
        val lo = sm.ordinal(left)
        val ro = sm.ordinal(right)
        Integer.compare(lo, ro) match
          case 0     => lat(lo).subsumption(left, right)
          case other => other < 0
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

      override def subsumption(left: T, right: T): Boolean = Range(0, lattices.productArity).forall { i =>
        lat(i).subsumption(left.productElement(i), right.productElement(i))
      }
    }
  }
}
