package tests.rescala.jvm

import rescala.interface.RescalaInterface
import tests.rescala.testtools.RETests

sealed trait ChangeX
case object DontSet      extends ChangeX
case object SetUnchanged extends ChangeX
case object SetChanged   extends ChangeX

class EvaluationOrderWithHigherOrderSignalsTest extends RETests {
  def run(engine: RescalaInterface, changeX: ChangeX): Unit = {
    import engine._

    val initialX = "initialValue"
    val newX     = if (changeX == SetChanged) "changedValue" else initialX

    val results = for (_ <- 0 to 10) yield {

      val x  = Var(initialX)(scheduler)
      val x4 = x.map(identity)(scheduler).map(identity)(scheduler).map(identity)(scheduler).map(identity)(scheduler)

      val ho                         = Var(x: Signal[String])(scheduler)
      var reevaluationRestartTracker = List.empty[String]
      val flatten = Signal.dynamic {
        val res = ho.value.value
        reevaluationRestartTracker ::= res
        res
      }(scheduler)

      changeX match {
        case DontSet => ho.set(x4)(implicitly, engine.ScopeSearch.fromSchedulerImplicit(engine.scheduler))
        case _ => transaction(x, ho) { implicit tx =>
            x.admit(newX)
            ho.admit(x4)
          }
      }

      // final value should be correct
      assert(flatten.readValueOnce == newX)
      // value should be determined by reevaluating twice after discovering a higher-level dependency on first run
      reevaluationRestartTracker
    }

    results.foreach { r =>
      assert(r.dropWhile(_ == newX).dropWhile(_ == initialX) == List())
    }
  }

  allEngines("dont set")(run(_, DontSet))
  allEngines("set unchanged")(run(_, SetUnchanged))
  allEngines("set changed")(run(_, SetChanged))
}
