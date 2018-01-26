package rescala.core.infiltration

import rescala.parrp.ParRP
import rescala.reactives.Signal

/** Accesses private[rescala] values for some low level tests */
object JVMInfiltrator {
  def unsafeNow[T](s: Signal[T, ParRP]): T = {
    s.state.current.get
  }
}
