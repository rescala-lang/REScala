package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.crdts.TwoPhaseSet
import com.github.ckuessner.encrdt.lattices.TwoPhaseSetLattice
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class TwoPhaseSetSpec extends AnyFlatSpec {

  "A TwoPhaseSetCrdt" should "merge with empty crdt" in {
    val left  = new TwoPhaseSet[String]("A")
    val right = new TwoPhaseSet[String]("B")

    assertResult(TwoPhaseSetLattice()) {
      left.merge(right.state)
      left.state
    }

  }

  it should "handle add correctly" in {
    val left = new TwoPhaseSet[Int]("A")

    assertResult(Set(1)) {
      left.add(1)
      left.values
    }

    for { i <- 1 to 100 } {
      assertResult((1 to i).toSet) {
        left.add(i)
        left.values
      }
    }
  }

  it should "handle simple remove correctly" in {
    val left = new TwoPhaseSet[Int]("A")

    left.add(1)

    assertResult(Set()) {
      left.remove(1)
      left.values
    }

    assertResult(Set(2)) {
      left.add(2)
      left.values
    }

    assertResult(Set()) {
      left.remove(2)
      left.values
    }

  }

  it should "not add element after removal of same element" in {
    val left = new TwoPhaseSet[Int]("A")
    left.add(1)
    left.remove(1)
    assertResult(Set()) {
      left.add(1)
      left.values
    }
  }

  /*
  implicit def twoPhaseIntSetCodec: JsonValueCodec[TwoPhaseSetLattice[Int]] = JsonCodecMaker.make
  it should "serialize and deserialize" in {
    val crdtState = TwoPhaseSetLattice[Int]()

    val serialized = writeToString(crdtState)
    val deserialized = readFromString[TwoPhaseSetLattice[Int]](serialized)

    deserialized shouldBe crdtState
  }
   */

}
