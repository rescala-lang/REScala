package kofre.base

import kofre.dotted.{Dotted, DottedDecompose}

import scala.annotation.targetName
import scala.collection.immutable.HashMap
import scala.compiletime.{erasedValue, summonAll, summonFrom, summonInline}
import scala.deriving.Mirror

type DecomposeLattice[A] = Lattice[A]
val DecomposeLattice: Lattice.type = Lattice

/** Well, its technically a semilattice, but that is just more to type. */
trait Lattice[A] {

  /** By assumption: associative, commutative, idempotent.
    *
    * For use with Delta CRDTs, this function should be optimized for the case
    * that left >> right, i.e., that left is the current state and right the delta
    */
  def merge(left: A, right: A): A

  /** Lattice order is derived from merge, but should be overridden for efficiency */
  def lteq(left: A, right: A): Boolean = merge(left, right) == right

  /** computes delta without state */
  def diff(state: A, delta: A): Option[A] = {
    decompose(delta).filter(!lteq(_, state)).reduceOption(merge)
  }

  /** Decompose a state into potentially smaller parts.
    * The only requirement is that merging the decomposed results produces the original state.
    * Requires a bottom to enable automatic decomposition of Product types
    * Note that the goal here is small individual storage size at reasonable computational cost.
    * Minimalism of returned results is not guaranteed.
    * It is also not guaranteed that the result does not overlap.
    */
  def decompose(a: A): Iterable[A] = Iterable(a)

  def bimap[B](to: A => B, from: B => A): Lattice[B] = new Lattice[B] {
    override def merge(left: B, right: B): B      = to(Lattice.this.merge(from(left), from(right)))
    override def lteq(left: B, right: B): Boolean = Lattice.this.lteq(from(left), from(right))
  }

  extension (left: A) {

    /** Lattice order is derived from merge, but should be overridden for efficiency. */
    def <=(right: A): Boolean = this.merge(left, right) == Lattice.normalize(right)(using this)
    @targetName("mergeInfix")
    def merge(right: A): A = this.merge(left, right)

    def decomposed: Iterable[A] = decompose(left)
  }
}

object Lattice {
  def apply[A](implicit ev: Lattice[A]): Lattice[A] = ev
  def merge[A: Lattice](left: A, right: A): A       = apply[A].merge(left, right)

  /** Merge functions can throw away redundant information, if one constructs values directly (not by using operators)
    * this could result in values that should be equal, but are not.
    * Normalize fixes this.
    */
  def normalize[A: Lattice](v: A): A = v merge v

  def decomposed[A](a: A)(implicit l: DecomposeLattice[A]): Iterable[A] = l.decomposed(a)

  implicit class Operators[A: Lattice](left: A):
    infix def merge(right: A): A = Lattice[A].merge(left, right)

  given latticeOrder[A: Lattice]: PartialOrdering[A] with {
    override def tryCompare(x: A, y: A): Option[Int] = {
      val lr = lteq(x, y)
      val rl = lteq(y, x)
      (lr, rl) match
        case (true, true)   => Some(0)
        case (false, false) => None
        case (true, false)  => Some(-1)
        case (false, true)  => Some(1)
    }

    override def lteq(x: A, y: A): Boolean = x <= y
  }

  // /////////////// common instances below ///////////////

  given intMaxLattice: Lattice[Int] = _ max _

  given setLattice[A]: Lattice[Set[A]] = new Lattice[Set[A]] {
    override def merge(left: Set[A], right: Set[A]): Set[A] = left union right
    override def lteq(left: Set[A], right: Set[A]): Boolean = left subsetOf right
    override def decompose(state: Set[A]): Iterable[Set[A]] = state.map(Set(_))
  }

  given optionLattice[A: Lattice]: Lattice[Option[A]] = new Lattice[Option[A]] {

    override def merge(left: Option[A], right: Option[A]): Option[A] = (left, right) match {
      case (None, r)          => r
      case (l, None)          => l
      case (Some(l), Some(r)) => Some(l merge r)
    }

    override def lteq(left: Option[A], right: Option[A]): Boolean = (left, right) match {
      case (None, _)          => true
      case (Some(_), None)    => false
      case (Some(l), Some(r)) => l <= r
    }

    override def decompose(state: Option[A]): Iterable[Option[A]] = state match {
      case None    => List.empty[Option[A]]
      case Some(v) => v.decomposed.map(Some(_))
    }

  }

  given mapLattice[K, V: Lattice]: Lattice[Map[K, V]] = new Lattice[Map[K, V]] {
    override def merge(left: Map[K, V], right: Map[K, V]): Map[K, V] =
      val (small, large) =
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
      left.keySet.forall { k =>
        left.get(k) <= right.get(k)
      }

    override def decompose(state: Map[K, V]): Iterable[Map[K, V]] = state.keys.flatMap { k =>
      Lattice[V].decompose(state(k)).map(v => Map(k -> v)) match {
        case s if s.isEmpty => List(Map(k -> state(k)))
        case s              => s
      }
    }
  }

  given functionLattice[K, V: Lattice]: Lattice[K => V] = (left, right) => k => left(k) merge right(k)

  given contextLattice[D: DottedDecompose]: Lattice[Dotted[D]] = (left, right) =>
    val dsMerged = DottedDecompose[D].mergePartial(left, right)
    val ccMerged = left.context merge right.context
    Dotted[D](dsMerged, ccMerged)

  given PairAsUIJDLattice[A: Lattice: Bottom, B: Lattice: Bottom]: Lattice[(A, B)]                          = derived
  given TripleAsUIJDLattice[A: Lattice: Bottom, B: Lattice: Bottom, C: Lattice: Bottom]: Lattice[(A, B, C)] = derived

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

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): DecomposeLattice[T] = {
    val lattices: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, DecomposeLattice]]
    val bottoms: Tuple  = summonAllMaybe[Tuple.Map[pm.MirroredElemTypes, Bottom]]
    new ProductDecomposeLattice[T](lattices, bottoms, pm, valueOf[pm.MirroredLabel])
  }

  class ProductDecomposeLattice[T <: Product](
      lattices: Tuple,
      bottoms: Tuple,
      pm: Mirror.ProductOf[T],
      label: String
  ) extends DecomposeLattice[T] {

    override def toString: String = s"ProductDecomposeLattice[${label}]"

    private def lat(i: Int): DecomposeLattice[Any] = lattices.productElement(i).asInstanceOf[DecomposeLattice[Any]]
    private def bot(i: Int, default: Any): Any =
      val btm = bottoms.productElement(i)
      if btm == null
      then default
      else btm.asInstanceOf[Bottom[Any]].empty

    override def merge(left: T, right: T): T =
      pm.fromProduct(new Product {
        def canEqual(that: Any): Boolean = false
        def productArity: Int            = lattices.productArity
        def productElement(i: Int): Any  = lat(i).merge(left.productElement(i), right.productElement(i))
      })

    override def decompose(a: T): Iterable[T] =
      Range(0, lattices.productArity).flatMap { j =>
        lat(j).decompose(a.productElement(j)).map { elem =>
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
