package test.collections

import org.scalatest._
import main.collections._
import react._

class ReactiveSetSpec extends FunSpec {
	describe("ReactiveHashSet") {
	    it("should allow fixed manipulation") {
	        val collection = new ReactiveHashSet[Int]
	        assertResult(false)(collection.contains(3)())
	        collection += 3
	        assertResult(true)(collection.contains(3)())
	    }
	    
	    it("should allow reactive checks") {
	        val collection = new ReactiveHashSet[Int]
	        val contains3 = collection.contains(3)
	        assertResult(false)(contains3())
	        collection += 3
	        assertResult(true)(contains3())
	    }
	    
	    it("should allow reactive inserts") {
	        val collection = new ReactiveHashSet[Int]
	        val contains3 = collection.contains(3)
	        val three = Var(2)
	        collection += three.toSignal
	        assertResult(false)(contains3())
	        three() = 3
	        assertResult(true)(collection.contains(3)())
	        assertResult(true)(contains3())
	        three() = 2
	        assertResult(false)(contains3())
	    }
	    
	    it("should allow mixing inserts") {
	        val collection = new ReactiveHashSet[Int]
	        val contains3 = collection.contains(3)
	        val three = Var(2)
	        
	        collection += three.toSignal
	        assertResult(false)(contains3())
	        collection += 3
	        assertResult(true)(contains3())
	        three() = 3
	        assertResult(true)(contains3())
	        three() = 2
	        assertResult(true)(contains3())
	    }
	    /*
	    it("should allow reactive removes") {
	        val collection = new ReactiveHashSet[Int]
	        val contains3 = collection.contains(3)
	        val three = Var(2)
	        
	        collection += 3
	        collection -= three.toSignal       
	        assertResult(true)(contains3())
	        three() = 3
	        assertResult(false)(contains3())
	        three() = 2
	        assertResult(true)(contains3())
	    }*/
	}
}