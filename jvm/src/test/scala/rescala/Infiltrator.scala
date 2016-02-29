package rescala

object Infiltrator {
  final def getLevel[S <: graph.LevelStruct](reactive: graph.Reactive[S])(implicit maybe: engines.Ticket[S]) = maybe {reactive.bud.level(_)}
  final def assertLevel[S <: graph.LevelStruct](reactive: graph.Reactive[S], level: Int, text: String = "level did not match")(implicit maybe: engines.Ticket[S]) =
    if (reactive.bud.isInstanceOf[graph.LevelStruct])
      assert(maybe {reactive.bud.level(_)} == level)

}
