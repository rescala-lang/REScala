package benchmarks.encrdt

import kofre.base.Id

implicit def idFromString(s: String): kofre.base.Id = kofre.base.Id.predefined(s)
