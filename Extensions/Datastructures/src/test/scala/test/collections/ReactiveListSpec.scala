package test.collections

import main.collections._
import org.scalatest._
import rescala._

import scala.language.implicitConversions

class ReactiveListSpec extends FunSpec {
  implicit def liftToSignal[T](v: T): Signal[T] = Var(v)

  describe("ReactiveList") {
        it("should provide a reactive size attribute") {
            val collection = new ReactiveList[Int](Var(List()))
            collection += 3
            val length = collection.size()
            assertResult(1, "initially incorrect")(length.now)
            collection += 4
            collection += 5
            assertResult(3, "incorrect after change")(length.now)
        }

        it("should allow for reactive access with fixed index") {
            val collection = new ReactiveList[Int](1,2,3,4,5)
            val secondElement = collection(1)
            assertResult(2, "initially incorrect")(secondElement.now)
            collection(1) = 4
            assertResult(4, "incorrect after change")(secondElement.now)
        }

        it("should allow for fully reactive appends") {
            val collection = new ReactiveList[Int](1,2,3)

            val a = Var(4)
            val b = Var(5)
            collection += a
            collection += b

            val fourthElement = collection(3) //Note: needs to be after the append, could be solved differently with Option
            val fifthElement = collection(4)

            assertResult(4)(fourthElement.now)
            assertResult(5)(fifthElement.now)

            a set 5
            b set 4
            assertResult(5)(fourthElement.now)
            assertResult(4)(fifthElement.now)
        }

        it("should allow for fully reactive updates") {
            val collection = new ReactiveList[Int](1,2,3,4,5)
            val idx = Var(0)
            val secondElement = collection(1)
            val idxElement = collection(idx)
            assertResult(2)(secondElement.now)
            assertResult(1)(idxElement.now)

            collection(idx) = 4
            assertResult(2)(secondElement.now)
            assertResult(4)(idxElement.now)

            idx set 1
            assertResult(4)(secondElement.now)
            assertResult(4)(idxElement.now)

            idx set 2
            assertResult(2)(secondElement.now)
            assertResult(4)(idxElement.now)
        }
	}
}
