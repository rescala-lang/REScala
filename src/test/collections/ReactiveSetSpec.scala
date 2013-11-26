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
	    
	    describe("'s filter method") {
	        it("should update the result reactively") {
	            val f = fixture
	            import f._
	            
	            val two = Var(2)
	            val three = Var(3)
	            collection += 1
	            collection += two.toSignal
	            collection += three.toSignal
	            
	            val filtered = collection.filter(_ % 2 == 0)
	            assertResult(true)(filtered.contains(2)())
	            assertResult(false)(filtered.contains(3)())
	            assertResult(false)(filtered.contains(4)())
	            
	            collection += 4
	            assertResult(true)(filtered.contains(4)())
	            
	            two() = 3
	            assertResult(false)(filtered.contains(2)())
	            
	            three() = 2
	            assertResult(true)(filtered.contains(2)())
	            
	            collection -= 4
	            assertResult(false)(filtered.contains(4)())
	        }
	        
	        it("should return a fully reactive collection") {
	        	val f = fixture
	            import f._
	            
	            val two = Var(2)
	            val three = Var(3)
	            
	            val filteredCollection = collection.filter(_ % 2 == 0)
	            val filteredContains2 = filteredCollection.contains(2)
	            
	            collection += two.toSignal
	            assertResult(true)(filteredContains2())
	            
	            filteredCollection -= two.toSignal
	            filteredCollection += three.toSignal
	            assertResult(false)(filteredContains2())
	            
	            three() = 2
	            assertResult(true)(filteredContains2())
	            
	            two() = 3
	            assertResult(false)(contains2())
	            assertResult(true)(filteredContains2())
	            
	            three() = 3
	            assertResult(false)(contains2())
	            assertResult(false)(filteredContains2())
	        }
	    }
	}
}