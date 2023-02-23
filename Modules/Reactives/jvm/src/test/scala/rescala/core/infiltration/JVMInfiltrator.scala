package rescala.core.infiltration

import rescala.interface.RescalaInterface
import rescala.parrp.ParRP

/** Accesses private[rescala] values for some low level tests */
object JVMInfiltrator {
  import rescala.Schedulers.parrp._
  def unsafeNow[T](s: Signal[T]): T = {
    s.state.current.get
  }
}
