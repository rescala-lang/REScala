package rescala.core.infiltration

import rescala.parrp.ParRPStruct
import rescala.operator.Signal

/** Accesses private[rescala] values for some low level tests */
object JVMInfiltrator {
  def unsafeNow[T](s: Signal[T, ParRPStruct]): T = {
    s.state.current.get
  }
}
