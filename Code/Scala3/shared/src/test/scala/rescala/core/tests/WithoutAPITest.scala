package rescala.core.tests

import rescala.core.ReName
import tests.rescala.testtools.RETests

class WithoutAPITest extends RETests {
  multiEngined { engine =>
    import engine._

    class CustomSource[T](initState: State[T]) extends ReSource with Interp[T] {
      outer =>

      override type Value = T
      override protected[rescala] def state: State[T]               = initState
      override protected[rescala] def name: ReName               = "I am a source name"
      override def interpret(v: Value): T                        = v
      override protected[rescala] def commit(base: Value): Value = base

      def makeChange(newValue: T) =
        new InitialChange {
          override val source: CustomSource.this.type = outer
          override def writeValue(base: source.Value, writeCallback: source.Value => Unit): Boolean = {
            if (base != newValue) {
              writeCallback(newValue)
              true
            } else false
          }
        }
    }

    class CustomDerivedString(
        initState: State[String],
        inputSource: Interp[String]
    ) extends Derived
        with Interp[String] {
      override type Value = String
      override protected[rescala] def state: State[Value]               = initState
      override protected[rescala] def name: ReName               = "I am a name"
      override protected[rescala] def commit(base: Value): Value = base

      override protected[rescala] def reevaluate(input: ReIn): Rout = {
        val sourceVal = input.dependStatic(inputSource)
        input.withValue(sourceVal + " :D")
      }

      override def interpret(v: Value): String = v
    }

    test("simple usage of core rescala without signals or events") {

      val customSource: CustomSource[String] =
        CreationTicket.fromScheduler(scheduler)
          .createSource("Hi!") { createdState =>
            new CustomSource[String](createdState)
          }

      assert(transaction(customSource) { _.now(customSource) } === "Hi!")

      val customDerived: Interp[String] =
        CreationTicket.fromScheduler(scheduler)
          .create(
            Set(customSource),
            "Well, this is an initial value",
            inite = false
          ) { createdState =>
            new CustomDerivedString(createdState, customSource)
          }

      assert(transaction(customSource) { _.now(customSource) } === "Hi!")
      assert(transaction(customDerived) { _.now(customDerived) } === "Well, this is an initial value")

      transaction(customSource) { _.recordChange(customSource.makeChange("Hello!")) }

      assert(transaction(customSource) { _.now(customSource) } === "Hello!")
      assert(transaction(customDerived) { _.now(customDerived) } === "Hello! :D")
    }
  }
}
