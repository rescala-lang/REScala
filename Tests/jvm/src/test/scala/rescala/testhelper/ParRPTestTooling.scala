package rescala.testhelper

import rescala.core.Engine
import rescala.parrp.ParRP
import rescala.reactives.Signal

object ParRPTestTooling {
  def unsafeNow[T](s: Signal[T, ParRP])(implicit engine: Engine[ParRP]): T = {
    engine.transaction()(t => t.creation.staticAfter(s).get)
  }
}
