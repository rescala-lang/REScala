package kofre.decompose

import kofre.base.Lattice
import kofre.base.Lattice.{Operators, mapLattice, optionLattice, setLattice}
import kofre.base.Bottom
import kofre.causality.CausalContext
import kofre.contextual.{WithContext, WithContextDecompose}

import scala.compiletime.summonAll
import scala.deriving.Mirror

trait Decompose[A] {

  /** Decomposes a lattice state into its unique irredundant join decomposition of join-irreducible states */
  def decompose(a: A): Iterable[A]
}

/** Extends the Lattice typeclass with the ability to compare states through unique irredundant join decomposition */
trait DecomposeLattice[A] extends Lattice[A], Bottom[A], Decompose[A] {

  /** computes delta without state */
  def diff(state: A, delta: A): Option[A] = {
    decompose(delta).filter(!lteq(_, state)).reduceOption(merge)
  }

}

/** reuse existing lattice instance to implement a DecomposeLattice */
trait DecomposeFromLattice[A](lattice: Lattice[A]) extends DecomposeLattice[A] {
  export lattice.merge
}

object DecomposeLattice {
  def apply[A](implicit l: DecomposeLattice[A]): DecomposeLattice[A] = l
  def bottom[A](implicit l: DecomposeLattice[A]): A                  = l.empty

  implicit class Operators[A: DecomposeLattice](left: A):
    @scala.annotation.targetName("lteq")
    def <=(right: A): Boolean  = DecomposeLattice[A].lteq(left, right)
    def decompose: Iterable[A] = DecomposeLattice[A].decompose(left)

  given IntAsUIJDLattice: DecomposeLattice[Int] = new DecomposeFromLattice[Int](_ max _) {
    override def lteq(left: Int, right: Int): Boolean = left <= right
    override def decompose(state: Int): Iterable[Int] = List(state)
    override def empty: Int                           = 0
  }

  given SetAsUIJDLattice[A]: DecomposeLattice[Set[A]] = new DecomposeFromLattice[Set[A]](setLattice) {
    override def lteq(left: Set[A], right: Set[A]): Boolean = left subsetOf right
    override def decompose(state: Set[A]): Iterable[Set[A]] = state.map(Set(_))
    override def empty: Set[A]                              = Set.empty[A]
  }

  given OptionAsUIJDLattice[A: DecomposeLattice]: DecomposeLattice[Option[A]] =
    new DecomposeFromLattice[Option[A]](optionLattice) {
      override def lteq(left: Option[A], right: Option[A]): Boolean = (left, right) match {
        case (None, _)          => true
        case (Some(_), None)    => false
        case (Some(l), Some(r)) => l <= r
      }

      override def decompose(state: Option[A]): Iterable[Option[A]] = state match {
        case None    => List.empty[Option[A]]
        case Some(v) => v.decompose.map(Some(_))
      }

      override def empty: Option[A] = Option.empty[A]
    }

  given MapAsUIJDLattice[K, V: DecomposeLattice]: DecomposeLattice[Map[K, V]] =
    new DecomposeFromLattice[Map[K, V]](mapLattice) {
      override def lteq(left: Map[K, V], right: Map[K, V]): Boolean =
        left.keySet.forall { k =>
          left.get(k) <= right.get(k)
        }

      override def decompose(state: Map[K, V]): Iterable[Map[K, V]] = state.keys.flatMap { k =>
        DecomposeLattice[V].decompose(state(k)).map(v => Map(k -> v)) match {
          case s if s.isEmpty => List(Map(k -> state(k)))
          case s              => s
        }
      }

      override def empty: Map[K, V] = Map.empty[K, V]
    }

  given PairAsUIJDLattice[A: DecomposeLattice, B: DecomposeLattice]: DecomposeLattice[(A, B)] = derived

  given TripleAsUIJDLattice[A: DecomposeLattice, B: DecomposeLattice, C: DecomposeLattice]
      : DecomposeLattice[(A, B, C)] = derived

  given contextUIJDLattice[D](using wcd: WithContextDecompose[D]): DecomposeLattice[WithContext[D]] =
    new DecomposeFromLattice[WithContext[D]](Lattice.contextLattice) {
      export wcd.decompose
      // needs manual override as export can not override :(
      override def lteq(left: WithContext[D], right: WithContext[D]): Boolean = wcd.lteq(left, right)
      override def empty: WithContext[D] = WithContext(wcd.empty, CausalContext.empty)
    }

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): DecomposeLattice[T] = {
    val lattices: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, DecomposeLattice]]
    val base            = Lattice.derived[T]
    new ProductDecomposeLattice[T](lattices, base, pm)
  }

  class ProductDecomposeLattice[T <: Product](lattices: Tuple, base: Lattice[T], pm: Mirror.ProductOf[T])
      extends DecomposeLattice[T] {
    export base.merge

    private def lat(i: Int): DecomposeLattice[Any] = lattices.productElement(i).asInstanceOf[DecomposeLattice[Any]]

    override def empty: T =
      pm.fromProduct(
        lattices.map[[α] =>> Any](
          [t] => (l: t) => l.asInstanceOf[kofre.decompose.DecomposeLattice[Any]].empty
        )
      )

    override def decompose(a: T): Iterable[T] =
      Range(0, lattices.productArity).flatMap { j =>
        lat(j).decompose(a.productElement(j)).map { elem =>
          pm.fromProduct(new Product {
            def canEqual(that: Any): Boolean = false
            def productArity: Int            = lattices.productArity
            def productElement(i: Int): Any  = if i == j then elem else lat(i).empty
          })
        }
      }

    override def lteq(left: T, right: T): Boolean = Range(0, lattices.productArity).forall { i =>
      lat(i).lteq(left.productElement(i), right.productElement(i))
    }
  }
}
