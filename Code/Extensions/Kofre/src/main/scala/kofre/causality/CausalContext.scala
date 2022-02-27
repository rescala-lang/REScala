package kofre

import kofre.Defs.{Id, Time}
import kofre.causality.impl.ArrayCausalContext

// so, this whole file is a bit of a hack to allow renaming of ArrayCausalContext to just CausalContext
// the TreeCausalContext can be used here as a drop in replacement for different performance behaviour.

package object causality {
  type CausalContext = ArrayCausalContext
}

package causality {

  object CausalContext {
    export ArrayCausalContext.*
  }
}
