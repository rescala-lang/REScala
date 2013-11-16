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
	}
}