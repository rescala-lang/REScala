package rescala.testhelper

import rescala.engine.Engine
import rescala.parrp.ParRP
import rescala.reactives.Signal

object ParRPTestTooling {
  def unsafeNow[T](s: Signal[T, ParRP])(implicit engine: Engine[ParRP, ParRP]): T = {
    engine.transaction()(t => t.after(s).get)
  }
}
