package reactives.core.infiltration

import reactives.parrp.ParRP

/** Accesses private[rescala] values for some low level tests */
object JVMInfiltrator {
  import reactives.default.*
  def unsafeNow[T](s: Signal[T]): T = {
    ???
//    s.state.current.get
  }
}
