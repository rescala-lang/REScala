package rescala.core.infiltration

import rescala.core.{Scheduler, ReSource}
import rescala.operator.Interface
import rescala.scheduler.Levelbased

/** Accesses private[rescala] values for some low level tests */
class Infiltrator(val api: Interface with Levelbased) {
  import api._
  // final def getLevel[S <: LevelStruct](reactive: graph.Reactive[S])(implicit maybe: CreationTicket[S]) = maybe {t => reactive.state.level(t.turn)}
  final def assertLevel(
                         reactive: ReSource.of[api.BundleState],
                         level: Int,
                         text: String = "level did not match"
  )(implicit maybe: Scheduler[api.BundleState]) =
    if (api.isInstanceOf[Levelbased] && reactive.state.isInstanceOf[LevelState[_]]) {
      reactive.state match {
        case rb: LevelState[_] => {
          val rblevel = maybe.forceNewTransaction() { _ =>
            rb.level()
          }
          assert(rblevel == level, s"$text, $reactive level was $rblevel but expected $level")
        }
      }
    }
}
