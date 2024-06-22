package benchmarks.encrdt

import rdts.base.Uid
import rdts.syntax.LocalUid

implicit def idFromString(s: String): Uid = rdts.base.Uid.predefined(s)
implicit def localidFromString(s: String): LocalUid = rdts.base.Uid.predefined(s)
