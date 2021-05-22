package rescala.core.infiltration

import rescala.interface.RescalaInterface
import rescala.parrp.ParRP

/** Accesses private[rescala] values for some low level tests */
class JVMInfiltrator(val api: RescalaInterface with ParRP) {
  import api._
  def unsafeNow[T](s: Signal[T]): T = {
    s.state.current.get
  }
}
