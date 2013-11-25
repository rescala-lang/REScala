package test.collections

import org.scalatest._
import main.collections._
import react._

class ReactiveSetSpec extends FunSpec {
    
    def fixture = new {
        val collection = new ReactiveHashSet[Int]
        val contains2 = collection.contains(2)
        val contains3 = collection.contains(3)
    }
    
	describe("ReactiveHashSet") {
	    it("should allow fixed manipulation") {
	        val f = fixture
	        import f._
	        assertResult(false)(collection.contains(3)())
	        collection += 3
	        assertResult(true)(collection.contains(3)())
	    }
	    
	    it("should allow reactive checks") {
	        val f = fixture
	        import f._
	        
	        assertResult(false)(contains3())
	        collection += 3
	        assertResult(true)(contains3())
	    }
	    
	    it("should allow reactive inserts") {
	        val f = fixture
	        import f._
	        val three = Var(2)
	        collection += three.toSignal
	        assertResult(false)(contains3())
	        three() = 3
	        assertResult(true)(contains3())
	        three() = 2
	        assertResult(false)(contains3())
	    }
	    
	    it("should allow mixing inserts") {
	        val f = fixture
	        import f._
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
	    
	    it("should allow mixing inserts with fix removal") {
	        val f = fixture
	        import f._
	        val three = Var(2)
	        collection += three.toSignal
	        assertResult(false)(contains3())
	        three() = 3
	        assertResult(true)(contains3())
	        collection -= 3
	        assertResult(false)(contains3())
	    }
	    
	    it("should allow reactive removal") {
	        val f = fixture
	        import f._
	        collection += 3
	        val remove = Var(2)
	        collection -= remove.toSignal
	        
	        assertResult(true)(contains3())
	        remove() = 3
	        assertResult(false)(contains3())
	        remove() = 2
	        assertResult(true)(contains3())
	    }
	    
	    describe("should allow mixing inserts with reactive removal") {
	        it("when a reactively added component gets removed before it gets inserted") {
	            val f = fixture
	            import f._
	            val add = Var(2)
	            val remove = Var(2)
	            
	            collection += 2
	            collection -= remove.toSignal
	            collection += add.toSignal
	            
	            assertResult(true)(contains2())
	            assertResult(false)(contains3())
	            
	            add() = 3
	            assertResult(false)(contains2())
	            assertResult(true)(contains3())
	            
	            remove() = 3
	            assertResult(true)(contains2())
	            assertResult(true)(contains3())
	        }
	        
	        it("when a reactively added component gets removed after it gets inserted") {
	            val f = fixture
	            import f._
	            
	            val add = Var(2)
	            val remove = Var(2)
	            
	            collection += add.toSignal
	            collection -= remove.toSignal
	            
	            assertResult(false)(contains2())
	            assertResult(false)(contains3())
	            
	            add() = 3
	            assertResult(false)(contains2())
	            assertResult(true)(contains3())
	            
	            remove() = 3
	            assertResult(false)(contains2())
	            assertResult(false)(contains3())
	        }
	    }
	}
}