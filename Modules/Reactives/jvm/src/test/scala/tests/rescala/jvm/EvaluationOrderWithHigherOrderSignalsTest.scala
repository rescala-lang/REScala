package tests.rescala.jvm

import reactives.operator.Interface
import tests.rescala.testtools.RETests

sealed trait ChangeX
case object DontSet      extends ChangeX
case object SetUnchanged extends ChangeX
case object SetChanged   extends ChangeX

class EvaluationOrderWithHigherOrderSignalsTest extends RETests {
  def run(engine: Interface, changeX: ChangeX): Unit = {
    import engine._

    val initialX = "initialValue"
    val newX     = if (changeX == SetChanged) "changedValue" else initialX

    val results = for (_ <- 0 to 10) yield {

      val x  = Var(initialX)
      val x4 = x.map(identity).map(identity).map(identity).map(identity)

      val ho                         = Var(x: Signal[String])
      var reevaluationRestartTracker = List.empty[String]
      val flatten = Signal.dynamic {
        val res = ho.value.value
        reevaluationRestartTracker ::= res
        res
      }

      changeX match {
        case DontSet => ho.set(x4)
        case _ => transaction(x, ho) { implicit tx =>
            x.admit(newX)(tx)
            ho.admit(x4)(tx)
          }
      }

      // final value should be correct
      assert(flatten.readValueOnce(scheduler) == newX)
      // value should be determined by reevaluating twice after discovering a higher-level dependency on first run
      reevaluationRestartTracker
    }

    results.foreach { r =>
      assert(r.dropWhile(_ == newX).dropWhile(_ == initialX) == List())
    }
  }

  test("dont set")(run(reactives.default, DontSet))
  test("set unchanged")(run(reactives.default, SetUnchanged))
  test("set changed")(run(reactives.default, SetChanged))
}
