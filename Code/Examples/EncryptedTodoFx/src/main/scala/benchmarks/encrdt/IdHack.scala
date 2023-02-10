package benchmarks.encrdt

import kofre.base.Uid

implicit def idFromString(s: String): kofre.base.Uid = kofre.base.Uid.predefined(s)
