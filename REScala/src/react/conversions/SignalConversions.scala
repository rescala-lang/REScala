package react.conversions

import react._


/**
 * Implicit conversions to make the syntax a bit more clean.
 * Use with care since they hide a bit what is going on.
 */
object SignalConversions {  
  // this is dangerous: do we want expressions to be constant or changing??
  implicit def toSignal[T](op: =>T): Signal[T] = SignalSynt((s: SignalSynt[T]) => (op))
  
  implicit def toVal[T](const: T): Signal[T] = SignalSynt((s: SignalSynt[T]) => (const))
  
}
