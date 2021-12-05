package rescala.core.infiltration

import rescala.interface.RescalaInterface
import rescala.parrp.ParRP

/** Accesses private[rescala] values for some low level tests */
class JVMInfiltrator[Api <: RescalaInterface with ParRP](val api: Api) {
  import api._
  def unsafeNow[T](s: Signal[T]): T = {
    s.state.current.get
  }
}
