package rescala

import rescala.levelbased.{LevelStruct, LevelStructType}
import rescala.graph.CreationTicket

object Infiltrator {
  final def getLevel[S <: LevelStruct](reactive: graph.Reactive[S])(implicit maybe: CreationTicket[S]) = maybe {t => reactive.state.level(t)}
  final def assertLevel[S <: graph.Struct](reactive: graph.Reactive[S], level: Int, text: String = "level did not match")(implicit maybe: CreationTicket[S]) =
    reactive.state match {
      case rb: LevelStructType[S @unchecked] => {
        val rblevel = maybe {t => rb.level(t)}
        assert(rblevel == level, s"$text, $reactive level was $rblevel but expected $level")
      }
      case _ =>
    }

}
