package test.abstraction

import org.scalatest._
import react._

class ProofOfConceptSpec extends FunSpec {
	describe("IntList") {
        it("should provide a reactive length") {
            val sut = new ProofOfConcept(List(1,2,3))
            val res = sut.length()
            assertResult(3)(res())
            sut.duplicate()
            assertResult(6)(res())
            sut.duplicate()
            assertResult(12)(res())
        } 
        
        it("should provide a constant head") {
            val sut = new ProofOfConcept(List(1,2,3))
            assertResult(1)(sut.head()())
        }
	}
}