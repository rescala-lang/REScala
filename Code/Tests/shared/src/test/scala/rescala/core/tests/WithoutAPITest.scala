package rescala.core.tests

import rescala.core.{InitialChange, Interp, ReName, CreationTicket => Ticket}
import tests.rescala.testtools.RETests

class WithoutAPITest extends RETests {
  multiEngined { engine =>
    import engine._

    class CustomSource[T](initState: ReStructure#State[T, ReStructure]) extends ReSource with Interp[T, ReStructure] {
      outer =>

      override type Value = T
      override protected[rescala] def state: State               = initState
      override protected[rescala] def name: ReName               = "I am a source name"
      override def interpret(v: Value): T                        = v
      override protected[rescala] def commit(base: Value): Value = base

      def makeChange(newValue: T) =
        new InitialChange[ReStructure] {
          override val source = outer
          override def writeValue(base: source.Value, writeCallback: source.Value => Unit): Boolean = {
            if (base != newValue) {
              writeCallback(newValue)
              true
            } else false
          }
        }
    }

    class CustomDerivedString(
        initState: ReStructure#State[String, ReStructure],
        inputSource: Interp[String, ReStructure]
    ) extends Derived
      with Interp[String, ReStructure] {
      override type Value = String
      override protected[rescala] def state: State               = initState
      override protected[rescala] def name: ReName               = "I am a name"
      override protected[rescala] def commit(base: Value): Value = base

      override protected[rescala] def reevaluate(input: ReIn): Rout = {
        val sourceVal = input.dependStatic(inputSource)
        input.withValue(sourceVal + " :D")
      }

      override def interpret(v: Value): String = v
    }

    test("simple usage of core recsala without signals or events") {

      val customSource: CustomSource[String] =
        Ticket.fromScheduler(scheduler)
          .createSource("Hi!") { createdState =>
            new CustomSource[String](createdState)
          }

      assert("Hi!" === transaction(customSource) { _.now(customSource) })

      val customDerived: Interp[String, ReStructure] =
        Ticket.fromScheduler(scheduler)
          .create(
            Set(customSource),
            "Well, this is an initial value",
            inite = false
          ) { createdState =>
            new CustomDerivedString(createdState, customSource)
          }

      assert("Hi!" === transaction(customSource) { _.now(customSource) })
      assert("Well, this is an initial value" === transaction(customDerived) { _.now(customDerived) })

      transaction(customSource) { _.recordChange(customSource.makeChange("Hello!")) }

      assert("Hello!" === transaction(customSource) { _.now(customSource) })
      assert("Hello! :D" === transaction(customDerived) { _.now(customDerived) })

    }
  }
}
