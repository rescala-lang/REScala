package rescala.core.infiltration

import rescala.interface.RescalaInterface
import rescala.scheduler.Levelbased

/** Accesses private[rescala] values for some low level tests */
class Infiltrator(val api: RescalaInterface with Levelbased) {
  import api._
  //final def getLevel[S <: LevelStruct](reactive: graph.Reactive[S])(implicit maybe: CreationTicket[S]) = maybe {t => reactive.state.level(t.turn)}
  final def assertLevel(
      reactive: ReSource,
      level: Int,
      text: String = "level did not match"
  )(implicit maybe: Scheduler) =
    if (api.isInstanceOf[Levelbased]) {
      reactive.state match {
        case rb: LevelState[_] => {
          val rblevel = maybe.forceNewTransaction() { at =>
            rb.level()
          }
          assert(rblevel == level, s"$text, $reactive level was $rblevel but expected $level")
        }
        case _ =>
      }
    }
}
