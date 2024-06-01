package lofi_acl.ardt.causality

import lofi_acl.ardt.causality.DotStore.*
import rdts.base.Bottom
import rdts.dotted.HasDots
import rdts.time.{Dot, Dots}

// See: Delta state replicated data types (https://doi.org/10.1016/j.jpdc.2017.08.003)
sealed trait DotStore[D] {
  def dots(dotStore: D): DotSet

  def empty: D
}

object DotStore {
  type DotSet       = Dots
  type DotFun[V]    = Map[Dot, V]
  type DotMap[K, V] = Map[K, V]

  inline def apply[D](using dotStore: DotStore[D]): DotStore[D] = dotStore

  given dotStoreCompat[A](using bot: Bottom[A], hd: HasDots[A]): DotStore[A] = new DotStore[A] {
    override def dots(dotStore: A): DotSet = hd.dots(dotStore)
    export bot.empty
  }

}
