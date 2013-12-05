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
	        val a = Var(2)
	        collection += a.toSignal
	        assertResult(false)(contains3())
	        a() = 3
	        assertResult(true)(contains3())
	        a() = 2
	        assertResult(false)(contains3())
	    }
	    
	    it("should allow mixing inserts") {
	        val f = fixture
	        import f._
	        val a = Var(2)
	        
	        collection += a.toSignal
	        assertResult(false)(contains3())
	        collection += 3
	        assertResult(true)(contains3())
	        a() = 3
	        assertResult(true)(contains3())
	        a() = 2
	        assertResult(true)(contains3())
	    }
	    
	    it("should allow mixing inserts with fix removal") {
	        val f = fixture
	        import f._
	        val a = Var(2)
	        collection += a.toSignal
	        assertResult(false)(contains3())
	        a() = 3
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
	            
	            val b = Var(2)
	            val a = Var(3)
	            collection += 1
	            collection += b.toSignal
	            collection += a.toSignal
	            
	            val filtered = collection.filter(_ % 2 == 0)
	            assertResult(true)(filtered.contains(2)())
	            assertResult(false)(filtered.contains(3)())
	            assertResult(false)(filtered.contains(4)())
	            
	            collection += 4
	            assertResult(true)(filtered.contains(4)())
	            
	            b() = 3
	            assertResult(false)(filtered.contains(2)())
	            
	            a() = 2
	            assertResult(true)(filtered.contains(2)())
	            
	            collection -= 4
	            assertResult(false)(filtered.contains(4)())
	        }
	        
	        it("should return a fully reactive collection") {
	        	val f = fixture
	            import f._
	            
	            val b = Var(2)
	            val a = Var(3)
	            
	            val filteredCollection = collection.filter(_ % 2 == 0)
	            val filteredContains2 = filteredCollection.contains(2)
	            
	            collection += b.toSignal
	            assertResult(true)(filteredContains2())
	            
	            filteredCollection -= b.toSignal
	            filteredCollection += a.toSignal
	            assertResult(false)(filteredContains2())
	            
	            a() = 2
	            assertResult(true)(filteredContains2())
	            
	            b() = 3
	            assertResult(false)(contains2())
	            assertResult(true)(filteredContains2())
	            
	            a() = 3
	            assertResult(false)(contains2())
	            assertResult(false)(filteredContains2())
	        }
	    }
	    describe("'s map method") {
	    	it("should return a fully reactive collection") {
	    	    val f = fixture
	            import f._
	            
	            val a = Var(3)
	            collection += 1
	            collection += a.toSignal
	            
	            val transformed = collection.map((_: Int) * 2)
	            val transformedContains2 = transformed.contains(2)
	            val transformedContains4 = transformed.contains(4)
	            val transformedContains6 = transformed.contains(6)
	            
	            assertResult(true)(transformedContains2())
	            assertResult(false)(transformedContains4())
	            assertResult(true)(transformedContains6())
	            
	            a() = 2
	            assertResult(true)(transformedContains2())
	            assertResult(true)(transformedContains4())
	            assertResult(false)(transformedContains6())
	            
	            
	            transformed -= collection.size
	            collection -= 2
	            assertResult(true)(transformedContains2())
	            assertResult(false)(transformedContains4())
	            assertResult(false)(transformedContains6())
	            
	            collection += 10
	            assertResult(false)(transformedContains2())
	    	}
	    }
	    
    	it("should have a functioning foldLeft method") { 
    	    val f = fixture
            import f._
            
            val a = Var(3)
            val b = Var(88)
            collection += 1
            collection += a.toSignal
            collection -= b.toSignal
            
            val sum = collection.foldLeft(0)(_+_)
            
            assertResult(4)(sum())
            
            a() = 2
            assertResult(3)(sum())
            
            collection += 3
            assertResult(6)(sum())
            
            b() = 1
            assertResult(5)(sum())
    	}
    
    	it("should have a functioning flatMap method") { 
    	    val f = fixture
            import f._
            
            val a = Var(3)
            val b = Var(9)
            collection += 1
            collection += a.toSignal
            collection -= b.toSignal
            
            val moments = collection.flatMap(x => List(x,x*x))
            val momentContains1 = moments.contains(1)
            val momentContains9 = moments.contains(9)
            val momentContains11 = moments.contains(11)
            
            assertResult(true)(momentContains1())
            assertResult(true)(momentContains9())
            assertResult(false)(momentContains11())
            
            b() = 3
            assertResult(false)(momentContains9())
    	}
	}
}
