package rescala

object Infiltrator {
  final def getLevel[S <: graph.LevelStruct](reactive: graph.Reactive[S])(implicit maybe: engines.Ticket[S]) = maybe {reactive.bud.level(_)}
  final def assertLevel[S <: graph.Struct](reactive: graph.Reactive[S], level: Int, text: String = "level did not match")(implicit maybe: engines.Ticket[S]) =
    reactive.bud match {
      case rb: graph.LevelSpore[_] => assert(maybe {rb.level(_)} == level)
      case _ =>
    }

}
