package rescala.core.infiltration

import rescala.operator.Interface
import rescala.parrp.ParRP

/** Accesses private[rescala] values for some low level tests */
object JVMInfiltrator {
  import rescala.scheduler.Schedulers.parrp._
  def unsafeNow[T](s: Signal[T]): T = {
    s.state.current.get
  }
}
