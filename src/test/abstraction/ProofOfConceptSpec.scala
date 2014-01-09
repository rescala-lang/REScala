package test.abstraction

import org.scalatest._
import react._

class ProofOfConceptSpec extends FunSpec {
	describe("IntList") {
        it("should provide a reactive length") {
            val sut = new ProofOfConcept(Set(1,2,3))
            val res = sut.size()
            assertResult(3)(res())
            sut += Var(4).toSignal
            assertResult(4)(res())
            sut += Var(5).toSignal
            sut += Var(6).toSignal
            assertResult(6)(res())
        } 
        
        it("should provide a constant head") {
            val sut = new ProofOfConcept(Set(1,2,3))
            assertResult(1)(sut.head()())
        }
        
        
        it("should allow reactive checks") {
            val sut = new ProofOfConcept(Set[Int]())
            val res = sut.contains(Var(3).toSignal)
            
            assertResult(false)(res())
            sut += Var(3).toSignal
            assertResult(true)(res())
        }
        
        it("should allow fully reactive inserts and removes") {
            val sut = new ProofOfConcept(Set(1,2,3))
            val contains3 = sut.contains(Var(3).toSignal)
            val contains4 = sut.contains(Var(4).toSignal)
            
            val a = Var(4)
            val b = Var(2)
            val c = Var(1)
            
            sut -= a.toSignal
            sut += b.toSignal
            sut -= c.toSignal
            
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
        	val sut = new ProofOfConcept(Set(1,2,3))
        	val filterResult = sut.filter(Var((_: Int) % 2 == 0).toSignal)
        	val mapResult = sut.map(Var((_: Int) * 2).toSignal)
        	val foldResult = sut.fold(Var((_: Int) + (_: Int)).toSignal, Var(1).toSignal)
        	val flatMapResult = sut.flatMap(Var((x: Int) => List(x, 2*x, 3*x)).toSignal)
        	
        	assertResult(Set(2))(filterResult())
        	assertResult(Set(2,4,6))(mapResult())
        	assertResult(7)(foldResult())
        	assertResult(Set(1,2,3,4,6,9))(flatMapResult())
        	
        	sut += Var(4).toSignal
        	
        	assertResult(Set(2,4))(filterResult())
        	assertResult(Set(2,4,6,8))(mapResult())
        	assertResult(11)(foldResult())
        	assertResult(Set(1,2,3,4,6,8,9,12))(flatMapResult())
        }
        
        
        it("should allow fully reactive higher order functions that modify self") {
            val sut = new ProofOfConcept(Set(1,2,3))
            val res = sut.contains(Var(3).toSignal)
            val f = Var((_: Int) % 2 == 0)
            
            assertResult(true)(res())
            
            sut.filterSelf(f.toSignal)
            assertResult(false)(res())
            
            f() = (_: Int) % 2 != 0
            assertResult(true)(res())
        }
	}
}