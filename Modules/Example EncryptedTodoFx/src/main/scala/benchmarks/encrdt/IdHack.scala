package benchmarks.encrdt

import rdts.base.Uid

implicit def idFromString(s: String): rdts.base.Uid = rdts.base.Uid.predefined(s)
