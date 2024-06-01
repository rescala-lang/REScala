package lofi_acl.ardt.causality

import rdts.base.Bottom
import rdts.dotted.HasDots
import rdts.time.{Dot, Dots}

type DotFun[V] = Map[Dot, V]
