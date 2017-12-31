package test.collections

import main.collections._
import org.scalatest._
import rescala._

import scala.language.implicitConversions

class ReactiveMapSpec extends FunSpec {
  implicit def liftToSignal[T](v: T): Signal[T] = Var(v)
  describe("HashMap") {
	    it("should allow fixed manipulation") {
	        val collection = new ReactiveHashMap[Int, Int]
	        assertResult(None)(collection.get(3).now)
	        collection += ((3,2))
	        assertResult(Some(2))(collection.get(3).now)
	    }

	    it("should have reactive size") {
	        val collection = new ReactiveHashMap[Int, Int]
	        val res = collection.size()
	        assertResult(0, "Fails before change")(res.now)
	        collection += 3 -> 2
	        assertResult(1, "Fails after change")(res.now)
	    }

	    it("should allow retrieval of reactive values") {
	        val collection = new ReactiveHashMap[Int, Int]
	        val res = collection.get(3)
	        assertResult(None, "Fails before change")(res.now)
	        collection += 3 -> 2
	        assertResult(Some(2), "Fails after change")(res.now)
	    }

	    it("should allow retrieval with reactive keys") {
	    	val collection = new ReactiveHashMap[Int, Int]
	    	val idx = Var(2)
	        val res = collection.get(idx)
	        assertResult(None)(res.now)
	        collection += 3 -> 2
	        assertResult(None)(res.now)
	        idx set 3
	        assertResult(Some(2))(res.now)
	    }

	    it("should allow reactive update") {
	        val collection = new ReactiveHashMap[Int, Int]
	        val res1 = collection.get(3)
	        val key1 = Var(2)
	        val pair1 = Var(3->2)
	        val value1 = Var(4)

	        collection += pair1
	        assertResult(Some(2))(res1.now)
	        collection.update(key1, value1)
	        assertResult(Some(2))(res1.now)
	        key1 set 3
	        assertResult(Some(4))(res1.now)
	        value1 set 1
	        assertResult(Some(1))(res1.now)
	        key1 set 2
	        assertResult(Some(2))(res1.now)
	    }

	    it("should allow mixed reactive retrieval") {
	        val collection = new ReactiveHashMap[Int, Int]
	        val a = collection.get(3)
	        val b = Var(3->2)
	        val c = collection.get(2)
	        val d = Var(4)
	        assertResult(None)(a.now)
	        collection += b
	        assertResult(Some(2))(a.now)
	        b set 3->1
	        assertResult(Some(1))(a.now)
	        assertResult(None)(c.now)
	        b set 2->1
	        assertResult(None)(a.now)
	        assertResult(Some(1))(c.now)
	        collection -= d
	        assertResult(Some(1))(c.now)
	        d set 2
	        assertResult(None)(c.now)
	        d set 3
	        assertResult(Some(1))(c.now)

	    }

	    it("should provide reactive higher order functions") {
	        val collection = new ReactiveHashMap[Int, Int]

	        val c = Var(3->3)
	        val filtered = collection.filter((p: (Int, Int)) => p._1 != p._2)
	        //val mapped = collection.map((p: (Int, Int)) => (p._1, p._1 * p._2))
	        //val folded = collection.foldLeft(0)(( sum: Int, p: (Int, Int)) => sum + p._2)
	        val filteredA = filtered.get(2)
	        val filteredB = filtered.get(3)
	        //val mappedA = mapped.get(3)

	        collection += 1->4
	        collection += 2->3
	        collection += c
	        collection += 4->1

	        assertResult(Some(3))(filteredA.now)
	        assertResult(None)(filteredB.now)
	        //assertResult(11)(folded.now)
	        //assertResult(Some(9))(mappedA.now)

	        c set 3->2
	        assertResult(Some(3))(filteredA.now)
	        assertResult(Some(2))(filteredB.now)
	        //assertResult(10)(folded.now)
	        //assertResult(Some(6))(mappedA.now)
	    }
	}
}
