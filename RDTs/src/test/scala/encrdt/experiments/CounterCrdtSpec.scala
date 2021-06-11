package de.ckuessner
package encrdt.experiments

import org.scalatest.flatspec.AnyFlatSpec

class CounterCrdtSpec extends AnyFlatSpec {

  "A CounterCrdt" should "initialize with 0" in {
    val crdt = new CounterCrdt(42)
    assertResult(0) {
      crdt.query()
    }
  }

  it should "increment on update with positive delta" in {
    val crdt = new CounterCrdt(42)
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
    val crdt = new CounterCrdt(42)
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
    val state = CounterCrdtState(Map(1 -> 10), Map(2 -> 2))
    val crdt = new CounterCrdt(1, state)
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

    crdt.merge(CounterCrdtState())
    assertResult(8) {
      crdt.query()
    }

  }

  it should "merge with all replicas in both crdts" in {
    val crdtLeft = new CounterCrdt(42)
    val crdtRight = new CounterCrdt(42)

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
    val leftStateInitial = CounterCrdtState(Map(1->1, 2->1))
    var crdtLeft = new CounterCrdt(1, leftStateInitial)
    val rightState = CounterCrdtState(Map(1->1, 2->2, 3->3, 4->4))

    assume(crdtLeft.query() == 2)

    // Only positives
    assertResult(10) {
      crdtLeft.merge(rightState)
      crdtLeft.query()
    }

    // Positive/Negative mixed
    val rightStateNegatives = CounterCrdtState(negativeCounts = Map(1->1, 2->2, 3->3, 4->4))
    assertResult(0) {
      crdtLeft.merge(rightStateNegatives)
      crdtLeft.query()
    }

    // Only negatives
    crdtLeft = new CounterCrdt(1)

    assertResult(-10) {
      crdtLeft.merge(rightStateNegatives)
      crdtLeft.query()
    }

  }

  it should "merge with some replicas not present in right crdt" in {
    val leftState = CounterCrdtState(Map(1->1, 2->1, 3->3, 4->4))
    var crdtLeft = new CounterCrdt(1, leftState)
    val rightState = CounterCrdtState(Map(1->1, 2->2))

    assume(crdtLeft.query() == 9)

    // Only positives
    assertResult(10) {
      crdtLeft.merge(rightState)
      crdtLeft.query()
    }

    // Positive/Negative mixed
    val leftStateNegativesMap = Map(1->1, 2->1, 3->3, 4->4)
    crdtLeft = new CounterCrdt(1, crdtLeft.state.copy(negativeCounts = leftStateNegativesMap))
    assume(crdtLeft.query() == 1)

    assertResult(0) {
      crdtLeft.merge(rightState.copy(negativeCounts = Map(1->0, 2->2)))
      crdtLeft.query()
    }
  }

  it should "allow the merge of two empty crdts" in {
    assertResult(CounterCrdtState()) {
      CounterCrdtState().merge(CounterCrdtState())
    }
  }

}
