package tests.rescala

import rescala.engine.{Engine, Turn}

sealed trait ChangeX
case object DontSet extends ChangeX
case object SetUnchanged extends ChangeX
case object SetChanged extends ChangeX

class UnchangedReevaluationLevelIncreaseTest extends RETests {
  def run(engine: Engine[S, Turn[S]], changeX: ChangeX): Unit = {
    import engine._

    val results = for(i <- 0 to 10) yield {
      val initialX = "asdf"
      val newX = if (changeX == SetChanged) "qwertz" else initialX

      val x = Var(initialX)
      val x4 = x.map(identity).map(identity).map(identity).map(identity)

      val ho = Var(x: Signal[String])
      var reevaluationRestartTracker = List.empty[String]
      val flatten = Signal {
        val x = ho()()
        reevaluationRestartTracker ::= x
        x
      }

      changeX match {
        case DontSet => ho.set(x4)
        case _ => engine.transaction(x, ho) { implicit t =>
          x.admit(newX)
          ho.admit(x4)
        }
      }

      // final value should be correct
      assert(flatten.now == newX)
      // value should be determined by reevaluating twice after discovering a higher-level dependency on first run
      reevaluationRestartTracker
    }

    val countedOutcomes = results.groupBy(x=>x).mapValues(_.size)
    assert(countedOutcomes.size == 1)
  }

  allEngines("dont set")(run(_, DontSet))
  allEngines("set unchanged")(run(_, SetUnchanged))
  allEngines("set changed")(run(_, SetChanged))
}
