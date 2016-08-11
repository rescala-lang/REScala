package test.abstraction

import org.scalatest._
import rescala._
import rescala.conversions.SignalConversions._

class SimpleReactiveSetSpec extends FunSpec {
	describe("IntList") {
        it("should provide a reactive length") {
            val sut = new SimpleReactiveSet(Set(1,2,3))
            val res = sut.size()
            assertResult(3)(res())
            sut += Var(4)
            assertResult(4)(res())
            sut += Var(5)
            sut += Var(6)
            assertResult(6)(res())
        } 
        
        it("should provide a constant head") {
            val sut = new SimpleReactiveSet(Set(1,2,3))
            assertResult(1)(sut.head()())
        }
        
        
        it("should allow reactive checks") {
            val sut = new SimpleReactiveSet(Set[Int]())
            val res = sut.contains(Var(3))
            
            assertResult(false)(res())
            sut += Var(3)
            assertResult(true)(res())
        }
        
        it("should allow fully reactive inserts and removes") {
            val sut = new SimpleReactiveSet(Set(1,2,3))
            val contains3 = sut.contains(Var(3))
            val contains4 = sut.contains(Var(4))
            
            val a = Var(4)
            val b = Var(2)
            val c = Var(1)
            
            sut -= a
            sut += b
            sut -= c
            
            assertResult(true)(contains3())
            assertResult(false)(contains4())
            
            a() = 3
            b() = 4
            assertResult(false)(contains3())
            assertResult(true)(contains4())
            
            c() = 4
            assertResult(false)(contains3())
            assertResult(false)(contains4())
            
            a() = 1
            assertResult(true)(contains3())
            assertResult(false)(contains4())
            
            c() = 3
            assertResult(false)(contains3())
            assertResult(true)(contains4())
            
            b() = 3
            assertResult(false)(contains3())
            assertResult(false)(contains4())
        }
        
        it("should allow fully reactive higher order functions") {
        	val sut = new SimpleReactiveSet(Set(1,2,3))
        	val filterResult: SimpleReactiveSet[Int] = sut.filter(Var((_: Int) % 2 == 0))
        	val mapResult = sut.map(Var((_: Int) * 2))
        	val foldResult = sut.fold(Var(1), Var((_: Int) + (_: Int)))
        	val flatMapResult = sut.flatMap(Var((x: Int) => List(x, 2*x, 3*x)))
        	
        	assertResult(Set(2))(filterResult().toValue)
        	assertResult(Set(2,4,6))(mapResult().toValue)
        	assertResult(7)(foldResult())
        	assertResult(Set(1,2,3,4,6,9))(flatMapResult())
        	
        	sut += Var(4)
        	
        	assertResult(Set(2,4))(filterResult().toValue)
        	assertResult(Set(2,4,6,8))(mapResult().toValue)
        	assertResult(11)(foldResult())
        	assertResult(Set(1,2,3,4,6,8,9,12))(flatMapResult())
        }
        
        
        it("should allow fully reactive higher order functions that modify self") {
            val sut = new SimpleReactiveSet(Set(1,2,3))
            val res = sut.contains(Var(3))
            val f = Var((_: Int) % 2 == 0)
            
            assertResult(true)(res())
            
            sut.filterSelf(f)
            assertResult(false)(res())
            
            f() = (_: Int) % 2 != 0
            assertResult(true)(res())
        }
	}
}