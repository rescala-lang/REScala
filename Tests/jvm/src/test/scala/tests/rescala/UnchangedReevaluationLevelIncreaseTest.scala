package tests.rescala

import rescala.engine.{Engine, Turn}

sealed trait ChangeX
case object DontSet extends ChangeX
case object SetUnchanged extends ChangeX
case object SetChanged extends ChangeX

class UnchangedReevaluationLevelIncreaseTest extends RETests {
  def run(engine: Engine[S, Turn[S]], changeX: ChangeX, whichX: Int): Unit = {
    import engine._

    val results = for(i <- 0 to 10) yield {
      val initialX = "asdf"
      val newX = if (changeX == SetChanged) "qwertz" else initialX

      val x = Var(initialX)
      val x1 = x.map(x => x)
      val x2 = x.map(x => x)
      val x3 = x.map(x => x)
      val xs = Array(x1, x2, x3)

      val ho = Var(x: Signal[String])
      var reevaluationRestartTracker = List.empty[String]
      val flatten = Signal {
        val x = ho()()
        reevaluationRestartTracker ::= x
        x
      }

      val newBase: Signal[String] = xs(whichX - 1)
      changeX match {
        case DontSet => ho.set(newBase)
        case _ => engine.transaction(x, ho) { implicit t =>
          x.admit(newX)
          ho.admit(newBase)
        }
      }
      engine.transaction(x, ho) { implicit t =>
        x.admit(newX)
        ho.admit(newBase)
      }

      // final value should be correct
      assert(flatten.now == newX)
      // value should be determined by reevaluating twice after discovering a higher-level dependency on first run
      reevaluationRestartTracker
    }

    val countedOutcomes = results.groupBy(x=>x).mapValues(_.size)
    assert(countedOutcomes.size == 1)
  }

  allEngines("dont set, x3")(run(_, DontSet, 3))
  allEngines("set unchanged, x3")(run(_, SetUnchanged, 3))
  allEngines("set changed, x3")(run(_, SetChanged, 3))

  allEngines("dont set, x2")(run(_, DontSet, 2))
  allEngines("set unchanged, x2")(run(_, SetUnchanged, 2))
  allEngines("set changed, x2")(run(_, SetChanged, 2))
}
