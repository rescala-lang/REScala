package reactives.core.infiltration

import reactives.core.{ReSource, Scheduler}
import reactives.operator.Interface
import reactives.scheduler.Levelbased
import reactives.scheduler.LevelbasedVariants.LevelState

/** Accesses private[rescala] values for some low level tests */
class Infiltrator(val api: Interface) {
  // final def getLevel[S <: LevelStruct](reactive: graph.Reactive[S])(using maybe: CreationTicket[S]) = maybe {t => reactive.state.level(t.turn)}
  final def assertLevel(
      reactive: ReSource,
      level: Int,
      text: String = "level did not match"
  )(using maybe: Scheduler[?]) =
    if api.isInstanceOf[Levelbased] && reactive.state.isInstanceOf[LevelState[?]] then {
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
