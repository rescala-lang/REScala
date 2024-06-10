package reactives.core.tests

import reactives.core.{CreationTicket, Derived, InitialChange, ReInfo, ReSource, ReadAs}
import reactives.SelectedScheduler.State
import tests.rescala.testtools.RETests

class WithoutAPITest extends RETests {
import reactives.default.*
{

    class CustomSource[T](initState: State[T]) extends reactives.core.ReSource with ReadAs[T] {
      outer =>

      override type State[V] = reactives.SelectedScheduler.State[V]

      override type Value = T
      override protected[reactives] def state: State[T]            = initState
      override val info: ReInfo                                    = ReInfo.create
      override def read(v: Value): T                               = v
      override protected[reactives] def commit(base: Value): Value = base

      def makeChange(newValue: T) =
        new InitialChange[State] {
          override val source: CustomSource.this.type = outer
          override def writeValue(base: source.Value, writeCallback: source.Value => Unit): Boolean = {
            if base != newValue then {
              writeCallback(newValue)
              true
            } else false
          }
        }
    }

    class CustomDerivedString(
        initState: State[String],
        inputSource: ReadAs.of[State, String]
    ) extends Derived
        with ReadAs[String] {
      override type Value    = String
      override type State[V] = reactives.SelectedScheduler.State[V]
      override protected[reactives] def state: State[Value]        = initState
      override val info: ReInfo                                    = ReInfo.create
      override protected[reactives] def commit(base: Value): Value = base

      override protected[reactives] def reevaluate(input: ReIn): Rout = {
        val sourceVal = input.dependStatic(inputSource)
        input.withValue(sourceVal + " :D")
      }

      override def read(v: Value): String = v
    }

    test("simple usage of core rescala without signals or events") {

      val customSource: CustomSource[String] =
        summon[CreationTicket[State]]
          .scope.createSource("Hi!") { createdState =>
            new CustomSource[String](createdState)
          }

      assertEquals(transaction(customSource) { at ?=> at.now(customSource) }, "Hi!")

      val customDerived: ReadAs.of[State, String] =
        summon[CreationTicket[State]]
          .scope.create(
            Set(customSource),
            "Well, this is an initial value",
            needsReevaluation = false
          ) { createdState =>
            new CustomDerivedString(createdState, customSource)
          }

      assertEquals(transaction(customSource) { at ?=> at.now(customSource) }, "Hi!")
      assertEquals(transaction(customDerived) { at ?=> at.now(customDerived) }, "Well, this is an initial value")

      transaction(customSource) { at ?=> at.recordChange(customSource.makeChange("Hello!")) }

      assertEquals(transaction(customSource) { at ?=> at.now(customSource) }, "Hello!")
      assertEquals(transaction(customDerived) { at ?=> at.now(customDerived) }, "Hello! :D")
    }
  }
}
