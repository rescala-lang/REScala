package rescala.testhelper

import rescala.core.Scheduler
import rescala.parrp.ParRP
import rescala.reactives.Signal

object ParRPTestTooling {
  def unsafeNow[T](s: Signal[T, ParRP])(implicit engine: Scheduler[ParRP]): T = {
    engine.transaction()(t => t.creation.staticAfter(s).get)
  }
}
