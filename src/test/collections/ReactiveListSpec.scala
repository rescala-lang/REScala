package test.collections

import org.scalatest._
import main.collections._
import react._

class ReactiveListSpec extends FunSpec {
	describe("ReactiveList") {
        it("should provide a reactive length attribute") {
            val collection = new ReactiveList[Int]
            collection.append(3)
            val length = collection.length
            assertResult(1, "initially incorrect")(length())
            collection.append(4, 5)
            assertResult(3, "incorrect after change")(length())
        }
	    
        it("should allow for reactive access with fixed index") {
            val collection = new ReactiveList[Int](1,2,3,4,5)
            val secondElement = collection(1)
            assertResult(2, "initially incorrect")(secondElement())
            collection(1) = 4
            assertResult(4, "incorrect after change")(secondElement())
        }
        
        it("should allow for fully reactive appends") {
            val collection = new ReactiveList[Int](1,2,3)
            
            val a = Var(4)
            val b = Var(5)
            collection.append(a.toSignal, b.toSignal)
            
            val fourthElement = collection(3) //Note: needs to be after the append, could be solved differently with Option
            val fifthElement = collection(4)
            
            assertResult(4)(fourthElement())
            assertResult(5)(fifthElement())
            
            a() = 5
            b() = 4
            assertResult(5)(fourthElement())
            assertResult(4)(fifthElement())
        }
        
        it("should allow for fully reactive updates") {
            val collection = new ReactiveList[Int](1,2,3,4,5)
            val idx = Var(0)
            val secondElement = collection(1)
            val idxElement = collection(idx.toSignal)
            assertResult(2)(secondElement())
            assertResult(1)(idxElement())
            
            collection(idx.toSignal) = 4
            assertResult(2)(secondElement())
            assertResult(4)(idxElement())
            
            idx() = 1
            assertResult(4)(secondElement())
            assertResult(4)(idxElement())
            
            idx() = 2
            assertResult(2)(secondElement())
            assertResult(4)(idxElement())
        }
	}
}