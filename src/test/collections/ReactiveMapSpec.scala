package test.collections

import org.scalatest._
import main.collections._
import react._

class ReactiveMapSpec extends FunSpec {
	describe("HashMap") {
	    it("should allow fixed manipulation") {
	        val collection = new ReactiveHashMap[Int, Int]
	        assertResult(None)(collection.get(3)())
	        collection += (3,2)
	        assertResult(Some(2))(collection.get(3)())
	    }
	    
	    it("should have reactive size") {
	        val collection = new ReactiveHashMap[Int, Int]
	        val res = collection.size
	        assertResult(0, "Fails before change")(res())
	        collection += 3 -> 2
	        assertResult(1, "Fails after change")(res())
	    }
	    
	    it("should allow retrieval of reactive values") {
	        val collection = new ReactiveHashMap[Int, Int]
	        val res = collection.get(3)
	        assertResult(None, "Fails before change")(res())
	        collection += 3 -> 2
	        assertResult(Some(2), "Fails after change")(res())
	    }
	    
	    it("should allow retrieval with reactive keys") {
	    	val collection = new ReactiveHashMap[Int, Int]
	    	val idx = Var(2)
	        val res = collection.get(idx.toSignal)	        
	        assertResult(None)(res())
	        collection += 3 -> 2
	        assertResult(None)(res())
	        idx() = 3
	        assertResult(Some(2))(res())
	    }
	}
}