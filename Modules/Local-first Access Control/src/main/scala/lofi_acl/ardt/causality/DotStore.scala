package lofi_acl.ardt.causality

import lofi_acl.ardt.causality.DotStore.*
import rdts.time.{Dot, Dots}

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
sealed trait DotStore[D] {
  def dots(dotStore: D): DotSet

  def bottom: D
}

object DotStore {
  type DotSet       = Dots
  type DotFun[V]    = Map[Dot, V]
  type DotMap[K, V] = Map[K, V]

  inline def apply[D](using dotStore: DotStore[D]): DotStore[D] = dotStore

  given dotSetDotStore: DotStore[DotSet] = new DotStore[DotSet] {
    override def dots(dotStore: DotSet): DotSet = dotStore

    override def bottom: DotSet = Dots.empty
  }

  // Todo: V should be a SemiLattice according to paper
  given dotFunDotStore[V]: DotStore[DotFun[V]] = new DotStore[DotFun[V]] {
    override def dots(dotFun: DotFun[V]): DotSet = Dots.from(dotFun.keySet)

    override def bottom: DotFun[V] = Map.empty
  }

  given dotMapDotStore[K, V: DotStore]: DotStore[DotMap[K, V]] = new DotStore[DotMap[K, V]] {
    override def dots(dotStore: DotMap[K, V]): DotSet =
      dotStore.values.map(DotStore[V].dots(_)).reduce((l, r) => l.union(r))

    override def bottom: DotMap[K, V] = Map.empty
  }
}
