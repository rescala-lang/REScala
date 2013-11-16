package test.collections

import org.scalatest._
import main.collections._
import react._

class ReactiveListSpec extends FunSpec {
	describe("ReactiveList") {
	    describe("'s constant functions") {
	        it("should return a reactive length") {
	            val collection = new ReactiveList[Int]
	            collection.append(3)
	            val length = collection.length
	            assertResult(1, "initially incorrect")(length())
	            collection.append(4, 5)
	            assertResult(3, "incorrect after change")(length())
	        }
	    }
	    
	    describe("'s basic mutating functions") {
	        it("should allow for reactive access with fixed index") {
	            val collection = new ReactiveList[Int](1,2,3,4,5)
	            val secondElement = collection(1)
	            assertResult(2, "initially incorrect")(secondElement())
	            collection(1) = 4
	            assertResult(4, "incorrect after change")(secondElement())
	        }
	        	it("should allow for reactive access with reactive index") {
	            val collection = new ReactiveList[Int](1,2,3,4,5)
	            val idx = Var(0)
	            val secondElement = collection(idx.toSignal)
	            assertResult(1, "initially incorrect")(secondElement())
	            idx() = 1
	            assertResult(2, "incorrect after change of index")(secondElement())
	            collection(1) = 4
	            assertResult(4, "incorrect after change of value")(secondElement())
	        }
	    }
	}
}