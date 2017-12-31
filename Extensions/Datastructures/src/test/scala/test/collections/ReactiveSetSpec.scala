package test.collections

import main.collections._
import org.scalatest._
import rescala._

import scala.language.implicitConversions

class ReactiveSetSpec extends FunSpec {
  implicit def liftToSignal[T](v: T): Signal[T] = Var(v)

  def fixture = new {
        val collection = new ReactiveHashSet[Int]
        val contains2 = collection.contains(2)
        val contains3 = collection.contains(3)
    }

	describe("ReactiveHashSet") {
	    it("should allow fixed manipulation") {
	        val f = fixture
	        import f._
	        assertResult(false)(collection.contains(3).now)
	        collection += 3
	        assertResult(true)(collection.contains(3).now)
	    }

	    it("should allow reactive checks") {
	        val f = fixture
	        import f._

	        assertResult(false)(contains3.now)
	        collection += 3
	        assertResult(true)(contains3.now)
	    }

	    it("should allow reactive inserts") {
	        val f = fixture
	        import f._
	        val a = Var(2)
	        collection += a
	        assertResult(false)(contains3.now)
	        a set 3
	        assertResult(true)(contains3.now)
	        a set 2
	        assertResult(false)(contains3.now)
	    }

	    it("should allow mixing inserts") {
	        val f = fixture
	        import f._
	        val a = Var(2)

	        collection += a
	        assertResult(false)(contains3.now)
	        collection += 3
	        assertResult(true)(contains3.now)
	        a set 3
	        assertResult(true)(contains3.now)
	        a set 2
	        assertResult(true)(contains3.now)
	    }

	    it("should allow mixing inserts with fix removal") {
	        val f = fixture
	        import f._
	        val a = Var(2)
	        collection += a
	        assertResult(false)(contains3.now)
	        a set 3
	        assertResult(true)(contains3.now)
	        collection -= 3
	        assertResult(false)(contains3.now)
	    }

	    it("should allow reactive removal") {
	        val f = fixture
	        import f._
	        collection += 3
	        val remove = Var(2)
	        collection -= remove

	        assertResult(true)(contains3.now)
	        remove set 3
	        assertResult(false)(contains3.now)
	        remove set 2
	        assertResult(true)(contains3.now)
	    }

	    describe("should allow mixing inserts with reactive removal") {
	        it("when a reactively added component gets removed before it gets inserted") {
	            val f = fixture
	            import f._
	            val add = Var(2)
	            val remove = Var(2)

	            collection += 2
	            collection -= remove
	            collection += add

	            assertResult(true)(contains2.now)
	            assertResult(false)(contains3.now)

	            add set 3
	            assertResult(false)(contains2.now)
	            assertResult(true)(contains3.now)

	            remove set 3
	            assertResult(true)(contains2.now)
	            assertResult(true)(contains3.now)
	        }

	        it("when a reactively added component gets removed after it gets inserted") {
	            val f = fixture
	            import f._

	            val add = Var(2)
	            val remove = Var(2)

	            collection += add
	            collection -= remove

	            assertResult(false)(contains2.now)
	            assertResult(false)(contains3.now)

	            add set 3
	            assertResult(false)(contains2.now)
	            assertResult(true)(contains3.now)

	            remove set 3
	            assertResult(false)(contains2.now)
	            assertResult(false)(contains3.now)
	        }
	    }

	    describe("'s filter method") {
	        it("should update the result reactively") {
	            val f = fixture
	            import f._

	            val b = Var(2)
	            val a = Var(3)
	            collection += 1
	            collection += b
	            collection += a

	            val filtered: ReactiveHashSet[Int] = collection.filter(Var((x: Int) => x % 2 == 0))
	            assertResult(true)(filtered.contains(2).now)
	            assertResult(false)(filtered.contains(3).now)
	            assertResult(false)(filtered.contains(4).now)

	            collection += 4
	            assertResult(true)(filtered.contains(4).now)

	            b set 3
	            assertResult(false)(filtered.contains(2).now)

	            a set 2
	            assertResult(true)(filtered.contains(2).now)

	            collection -= 4
	            assertResult(false)(filtered.contains(4).now)
	        }

	        it("should return a fully reactive collection") {
	        	val f = fixture
	            import f._

	            val b = Var(2)
	            val a = Var(3)

	            val filteredCollection = collection.filter(Var((x: Int) => x % 2 == 0))
	            val filteredContains2 = filteredCollection.contains(2)

	            collection += b
	            assertResult(true)(filteredContains2.now)

	            filteredCollection -= b
	            filteredCollection += a
	            assertResult(false)(filteredContains2.now)

	            a set 2
	            assertResult(true)(filteredContains2.now)

	            b set 3
	            assertResult(false)(contains2.now)
	            assertResult(true)(filteredContains2.now)

	            a set 3
	            assertResult(false)(contains2.now)
	            assertResult(false)(filteredContains2.now)
	        }
	    }
	    describe("'s map method") {
	    	it("should return a fully reactive collection") {
	    	    val f = fixture
	            import f._

	            val a = Var(3)
	            collection += 1
	            collection += a

	            val transformed = collection.map(Var((x: Int) => x * 2))
	            val transformedContains2 = transformed.contains(2)
	            val transformedContains4 = transformed.contains(4)
	            val transformedContains6 = transformed.contains(6)

	            assertResult(true)(transformedContains2.now)
	            assertResult(false)(transformedContains4.now)
	            assertResult(true)(transformedContains6.now)

	            a set 2
	            assertResult(true)(transformedContains2.now)
	            assertResult(true)(transformedContains4.now)
	            assertResult(false)(transformedContains6.now)


	            transformed -= collection.size()
	            collection -= 2
	            assertResult(true)(transformedContains2.now)
	            assertResult(false)(transformedContains4.now)
	            assertResult(false)(transformedContains6.now)

	            collection += 10
	            assertResult(false)(transformedContains2.now)
	    	}
	    }

    	it("should have a functioning foldLeft method") {
    	    val f = fixture
            import f._

            val a = Var(3)
            val b = Var(88)
            collection += 1
            collection += a
            collection -= b

            val sum = collection.foldLeft(0)(Var((x: Int, y: Int) => x + y))

            assertResult(4)(sum.now)

            a set 2
            assertResult(3)(sum.now)

            collection += 3
            assertResult(6)(sum.now)

            b set 1
            assertResult(5)(sum.now)
    	}

    	it("should have a functioning flatMap method") {
    	    val f = fixture
            import f._

            val a = Var(3)
            val b = Var(9)
            collection += 1
            collection += a
            collection -= b

            val moments = collection.flatMap(Var((x: Int) => List(x,x*x)))
            val momentContains1 = moments.contains(1)
            val momentContains9 = moments.contains(9)
            val momentContains11 = moments.contains(11)

            assertResult(true)(momentContains1.now)
            assertResult(true)(momentContains9.now)
            assertResult(false)(momentContains11.now)

            b set 3
            assertResult(false)(momentContains9.now)
    	}
	}
}
