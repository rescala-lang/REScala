package lofi_acl.ardt.causality

import lofi_acl.ardt.causality.DotStore.*
import rdts.base.Bottom
import rdts.dotted.HasDots
import rdts.time.{Dot, Dots}


object DotStore {
  type DotFun[V]    = Map[Dot, V]
  type DotMap[K, V] = Map[K, V]
}
