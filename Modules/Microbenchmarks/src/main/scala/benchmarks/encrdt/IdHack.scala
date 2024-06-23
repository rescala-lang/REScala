package benchmarks.encrdt

import rdts.base.{LocalUid, Uid}

implicit def idFromString(s: String): Uid           = rdts.base.Uid.predefined(s)
implicit def localidFromString(s: String): LocalUid = rdts.base.Uid.predefined(s)
