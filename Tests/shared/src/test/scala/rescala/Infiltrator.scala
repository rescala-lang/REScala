package rescala

import rescala.core.{Engine, Struct}
import rescala.levelbased.LevelStructType

object Infiltrator {
  //final def getLevel[S <: LevelStruct](reactive: graph.Reactive[S])(implicit maybe: CreationTicket[S]) = maybe {t => reactive.state.level(t.turn)}
  final def assertLevel[S <: Struct](reactive: core.Node[S], level: Int, text: String = "level did not match")(implicit maybe: Engine[S]) =
    reactive.state match {
      case rb: LevelStructType[S @unchecked] => {
        val rblevel = maybe.transaction(){at =>
           rb.level(at.creation.asInstanceOf[core.Turn[S]])
        }
        assert(rblevel == level, s"$text, $reactive level was $rblevel but expected $level")
      }
      case _ =>
    }

}
