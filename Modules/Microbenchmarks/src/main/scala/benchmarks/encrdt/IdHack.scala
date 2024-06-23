package benchmarks.encrdt

import rdts.base.{LocalUid, Uid}

given idFromString: Conversion[String, Uid]           = rdts.base.Uid.predefined
given localidFromString: Conversion[String, LocalUid] = rdts.base.LocalUid.predefined
