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
	    
	    it("should allow reactive update") {
	        val collection = new ReactiveHashMap[Int, Int]
	        val res1 = collection.get(3)
	        val key1 = Var(2)
	        val pair1 = Var(3->2)
	        val value1 = Var(4)
	        
	        collection += pair1.toSignal
	        assertResult(Some(2))(res1())
	        collection.update(key1.toSignal, value1.toSignal)
	        assertResult(Some(2))(res1())
	        key1() = 3
	        assertResult(Some(4))(res1())
	        value1() = 1
	        assertResult(Some(1))(res1())
	        key1() = 2
	        assertResult(Some(2))(res1())
	    }
	    
	    it("should allow mixed reactive retrieval") {
	        val collection = new ReactiveHashMap[Int, Int]
	        val a = collection.get(3)
	        val b = Var(3->2)
	        val c = collection.get(2)
	        val d = Var(4)
	        assertResult(None)(a())
	        collection += b.toSignal
	        assertResult(Some(2))(a())
	        b() = 3->1
	        assertResult(Some(1))(a())
	        assertResult(None)(c())
	        b() = 2->1
	        assertResult(None)(a())
	        assertResult(Some(1))(c())
	        collection -= d.toSignal
	        assertResult(Some(1))(c())
	        d() = 2
	        assertResult(None)(c())
	        d() = 3
	        assertResult(Some(1))(c())
	        
	    }
	}
}