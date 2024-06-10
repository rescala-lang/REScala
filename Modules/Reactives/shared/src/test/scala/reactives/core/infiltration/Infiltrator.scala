package reactives.core.infiltration

import reactives.core.{ReSource, Scheduler}
import reactives.scheduler.Levelbased
import reactives.scheduler.LevelbasedVariants.LevelState

import scala.annotation.nowarn

/** Accesses private[rescala] values for some low level tests */
class Infiltrator {
  // final def getLevel[S <: LevelStruct](reactive: graph.Reactive[S])(using maybe: CreationTicket[S]) = maybe {t => reactive.state.level(t.turn)}
  val api = reactives.default
  final def assertLevel(
      reactive: ReSource,
      level: Int,
      text: String = "level did not match"
  )(using maybe: Scheduler[?]) =
    if (api.isInstanceOf[Levelbased] && reactive.state.isInstanceOf[LevelState[?]]) : @nowarn then {
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
