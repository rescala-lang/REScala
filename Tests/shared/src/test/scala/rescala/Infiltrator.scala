package rescala

import rescala.engine.Engine
import rescala.levelbased.LevelStructType

object Infiltrator {
  //final def getLevel[S <: LevelStruct](reactive: graph.Reactive[S])(implicit maybe: CreationTicket[S]) = maybe {t => reactive.state.level(t.turn)}
  final def assertLevel[S <: graph.Struct](reactive: graph.Reactive[S], level: Int, text: String = "level did not match")(implicit maybe: Engine[S]) =
    reactive.state match {
      case rb: LevelStructType[S @unchecked] => {
        val rblevel = maybe.transaction(){at =>
           rb.level(at.creation.asInstanceOf[engine.Turn[S]])
        }
        assert(rblevel == level, s"$text, $reactive level was $rblevel but expected $level")
      }
      case _ =>
    }

}
