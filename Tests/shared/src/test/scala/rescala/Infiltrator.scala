package rescala

import rescala.levelbased.{LevelStruct, LevelStructType}

object Infiltrator {
  final def getLevel[S <: LevelStruct](reactive: graph.Reactive[S])(implicit maybe: engines.TurnSource[S]) = maybe {reactive.state.level(_)}
  final def assertLevel[S <: graph.Struct](reactive: graph.Reactive[S], level: Int, text: String = "level did not match")(implicit maybe: engines.TurnSource[S]) =
    reactive.state match {
      case rb: LevelStructType[_] => {
        val rblevel = maybe {rb.level(_)}
        assert(rblevel == level, s"$text, $reactive level was $rblevel but expected $level")
      }
      case _ =>
    }

}
