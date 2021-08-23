package de.ckuessner
package encrdt.lattices

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString}
import de.ckuessner.encrdt.lattices.interfaces.SemiLattice
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CounterSpec extends AnyFlatSpec {

  "A CounterCrdt" should "initialize with 0" in {
    val crdt = new Counter("42")
    assertResult(0) {
      crdt.query()
    }
  }

  it should "increment on update with positive delta" in {
    val crdt = new Counter("42")
    crdt.update(10)
    assertResult(10) {
      crdt.query()
    }
    crdt.update(11)
    assertResult(21) {
      crdt.query()
    }
    crdt.update(21)
    assertResult(42) {
      crdt.query()
    }
  }

  it should "decrement on update with negative delta" in {
    val crdt = new Counter("42")
    crdt.update(10)
    crdt.update(-1)
    assertResult(9) {
      crdt.query()
    }

    crdt.update(-9)
    assertResult(0) {
      crdt.query()
    }

    crdt.update(-9)
    assertResult(-9) {
      crdt.query()
    }
  }

  it should "be idempotent when merging" in {
    val state = CounterCrdtLattice(Map("1" -> 10), Map("2" -> 2))
    val crdt = new Counter("1", state)
    assume(crdt.query() == 8)

    crdt.merge(state)
    assertResult(8) {
      crdt.query()
    }

    crdt.merge(state)
    assertResult(8) {
      crdt.query()
    }

    crdt.merge(state.copy(negativeCounts = Map()))
    assertResult(8) {
      crdt.query()
    }

    crdt.merge(state.copy(positiveCounts = Map()))
    assertResult(8) {
      crdt.query()
    }

    crdt.merge(CounterCrdtLattice())
    assertResult(8) {
      crdt.query()
    }

  }

  it should "merge with all replicas in both crdts" in {
    val crdtLeft = new Counter("42")
    val crdtRight = new Counter("42")

    crdtLeft.update(10)
    crdtRight.update(20)

    assertResult(20) {
      crdtLeft.merge(crdtRight.state)
      crdtLeft.query()
    }

    assertResult(20) {
      crdtRight.merge(crdtLeft.state)
      crdtRight.query()
    }
  }

  it should "merge with some replicas not present in left crdt" in {
    val leftStateInitial = CounterCrdtLattice(Map("1" -> 1, "2" -> 1))
    var crdtLeft = new Counter("1", leftStateInitial)
    val rightState = CounterCrdtLattice(Map("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4))

    assume(crdtLeft.query() == 2)

    // Only positives
    assertResult(10) {
      crdtLeft.merge(rightState)
      crdtLeft.query()
    }

    // Positive/Negative mixed
    val rightStateNegatives = CounterCrdtLattice(negativeCounts = Map("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4))
    assertResult(0) {
      crdtLeft.merge(rightStateNegatives)
      crdtLeft.query()
    }

    // Only negatives
    crdtLeft = new Counter("1")

    assertResult(-10) {
      crdtLeft.merge(rightStateNegatives)
      crdtLeft.query()
    }

  }

  it should "merge with some replicas not present in right crdt" in {
    val leftState = CounterCrdtLattice(Map("1" -> 1, "2" -> 1, "3" -> 3, "4" -> 4))
    var crdtLeft = new Counter("1", leftState)
    val rightState = CounterCrdtLattice(Map("1" -> 1, "2" -> 2))

    assume(crdtLeft.query() == 9)

    // Only positives
    assertResult(10) {
      crdtLeft.merge(rightState)
      crdtLeft.query()
    }

    // Positive/Negative mixed
    val leftStateNegativesMap = Map("1" -> 1, "2" -> 1, "3" -> 3, "4" -> 4)
    crdtLeft = new Counter("1", crdtLeft.state.copy(negativeCounts = leftStateNegativesMap))
    assume(crdtLeft.query() == 1)

    assertResult(0) {
      crdtLeft.merge(rightState.copy(negativeCounts = Map("1" -> 0, "2" -> 2)))
      crdtLeft.query()
    }
  }

  it should "allow the merge of two empty crdts" in {
    assertResult(CounterCrdtLattice()) {
      SemiLattice.merged(CounterCrdtLattice(), CounterCrdtLattice())
    }
  }

  it should "serialize and deserialize" in {
    val crdtState = CounterCrdtLattice(positiveCounts = Map("42" -> 21, "21" -> 42), negativeCounts = Map("42" -> 42, "21" -> 21))

    val serializedState = writeToString(crdtState)
    val crdtDeserialized = readFromString[CounterCrdtLattice](serializedState)

    crdtDeserialized shouldBe crdtState
  }


}
