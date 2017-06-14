package rescala

import rescala.graph.CreationTicket
import rescala.levelbased.LevelStructType

object Infiltrator {
  //final def getLevel[S <: LevelStruct](reactive: graph.Reactive[S])(implicit maybe: CreationTicket[S]) = maybe {t => reactive.state.level(t.turn)}
  final def assertLevel[S <: graph.Struct](reactive: graph.Reactive[S], level: Int, text: String = "level did not match")(implicit maybe: CreationTicket[S]) =
    reactive.state match {
      case rb: LevelStructType[S @unchecked] => {
        val rblevel = maybe {t => rb.level(t.turn)}
        assert(rblevel == level, s"$text, $reactive level was $rblevel but expected $level")
      }
      case _ =>
    }

}
