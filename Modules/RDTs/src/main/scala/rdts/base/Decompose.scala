package rdts.base

import scala.collection.immutable.MapOps
import scala.compiletime.summonAll
import scala.deriving.Mirror

@FunctionalInterface
trait Decompose[A] {

  /** Decompose a state into potentially smaller parts.
    * Guarantees for any two states a and b that `decompose(a).fold(b)(merge) == b `merge` a`, i.e., merging the decomposed values into b has the same result as merging the full a into b (assuming b is normalized).
    *
    * Note that the goal here is small individual storage size at reasonable computational cost.
    * The results might not be minimal.
    * The results might overlap.
    * The result may be the empty sequence if `a` is the bottom value.
    * Each result is <= the original value (according to the lattice).
    */
  extension (a: A) {
    def decomposed: Iterable[A]
  }

}

object Decompose {

  def decompose[A: Decompose](a: A): Iterable[A] = a.decomposed

  def atomic[A]: Decompose[A] = a => Iterable(a)

  given intDecompose: Decompose[Int] = atomic

  given mapDecompose[K, V: Decompose, Mp[K1, +V1] <: MapOps[K1, V1, Mp, Mp[K1, V1]]]: Decompose[Mp[K, V]] with {
    extension (state: Mp[K, V])
      override def decomposed: Iterable[Mp[K, V]] =
        for
          case (k, v) <- state
          d <- v.decomposed
        yield state.mapFactory(k -> d)
  }

  given setDecompose[A]: Decompose[Set[A]] = a => a.map(e => Set(e))

  inline given tupleDecompose[T <: Tuple](using pm: Mirror.ProductOf[T]): Decompose[T] = derived

  inline def sumDecompose[T](using sm: Mirror.SumOf[T]): Decompose[T] =
    val lattices: Tuple = summonAll[Tuple.Map[sm.MirroredElemTypes, Decompose]]
    new Derivation.SumDecompose[T](sm, lattices)

  inline def derived[T <: Product](using pm: Mirror.ProductOf[T]): Decompose[T] = {
    val lattices: Tuple = summonAll[Tuple.Map[pm.MirroredElemTypes, Decompose]]
    val bottoms: Tuple  = Lattice.Derivation.summonAllMaybe[Tuple.Map[pm.MirroredElemTypes, Bottom]]
    new Derivation.ProductDecompose[T](lattices, bottoms, pm, valueOf[pm.MirroredLabel])
  }

  object Derivation {
    class SumDecompose[T](sm: Mirror.SumOf[T], lattices: Tuple) extends Decompose[T] {

      private def lat(i: Int): Decompose[T] = lattices.productElement(i).asInstanceOf[Decompose[T]]

      extension (a: T) {
        override def decomposed: Iterable[T] =
          val ordinal = sm.ordinal(a)
          val res     = lat(ordinal).decomposed(a)
          // When `a` decomposes into nothing, it is no longer possible to distinguish which alternative of the sum we are dealing with. That is fine when the ordinal is 0 because then we have reached the bottom case for the sum type, but in all other cases we must keep enough information around to figure out the ordinal.
          if ordinal != 0 && res.isEmpty
          then Iterable(a)
          else res
      }
    }

    class ProductDecompose[T <: Product](
        lattices: Tuple,
        bottoms: Tuple,
        pm: Mirror.ProductOf[T],
        label: String
    ) extends Decompose[T] {

      override def toString: String = s"ProductDecompose[${label}]"

      private def lat(i: Int): Decompose[Any] = lattices.productElement(i).asInstanceOf[Decompose[Any]]
      private def bot(i: Int, default: Any): Any =
        val btm = bottoms.productElement(i)
        if btm == null
        then default
        else btm.asInstanceOf[Bottom[Any]].empty
      private def isEmpty(i: Int)(a: Any): Boolean =
        val btm = bottoms.productElement(i)
        if btm == null then false
        else btm.asInstanceOf[Bottom[Any]].isEmpty(a)

      extension (a: T)
        override def decomposed: Iterable[T] = {
          // Singleton types (product arity == 0) would return an empty iterable if not handled explicitly.
          // That would be “fine” with regards to the guarantees of decompose, but is slightly less useful in cases where the singleton type does not have a bottom instance defined.
          if lattices.productArity == 0 then Iterable(a)
          else
            Range(0, lattices.productArity).flatMap { j =>
              lat(j).decomposed(a.productElement(j)).iterator.filterNot(isEmpty(j)).map { elem =>
                pm.fromProduct(new Product {
                  def canEqual(that: Any): Boolean = false
                  def productArity: Int            = lattices.productArity
                  def productElement(i: Int): Any = if i == j then elem
                  else bot(i, a.productElement(i))
                })
              }
            }
        }

    }
  }
}
