package com.github.ckuessner.encrdt.crdts.delta

import com.github.ckuessner.encrdt.crdts.DeltaAddWinsLastWriterWinsMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class DeltaAddWinsLastWriterWinsMapSpec extends AnyFlatSpec {
  def awlwwm(replicaId: String = "A"): DeltaAddWinsLastWriterWinsMap[Int, Int] =
    new DeltaAddWinsLastWriterWinsMap[Int, Int](replicaId)

  def genMap(from: Int, to: Int): Map[Int, Int] =
    (from to to).map(i => i -> i).toMap

  "put" should "work with empty map" in {
    val crdt = awlwwm()
    crdt.put(1, 42)
    crdt.values should ===(Map(1 -> 42))
  }

  it should "work after remove of same element" in {
    val crdt = awlwwm()
    crdt.put(1, 1)
    crdt.remove(1)
    crdt.put(1, 42)
    crdt.values should ===(Map(1 -> 42))
  }

  it should "overwrite values" in {
    val crdt = awlwwm()
    crdt.put(1, 1)
    crdt.put(1, 42)
    crdt.values should ===(Map(1 -> 42))
  }

  it should "work with multiple insertions" in {
    val crdt = awlwwm()
    crdt.put(1, 1)
    crdt.values should ===(Map(1 -> 1))
    crdt.put(2, 2)
    crdt.values should ===(Map(1 -> 1, 2 -> 2))
    crdt.put(3, 3)
    crdt.values should ===(Map(1 -> 1, 2 -> 2, 3 -> 3))
  }

  "remove" should "work with empty map" in {
    val crdt = awlwwm()
    crdt.remove(1)
    crdt.values should ===(Map.empty)
    crdt.remove(1)
    crdt.values should ===(Map.empty)
  }

  it should "remove the element" in {
    val crdt = awlwwm()
    crdt.put(1, 1)
    crdt.remove(1)
    crdt.values should ===(Map.empty)
  }

  it should "remove only element that is removed" in {
    val crdt = awlwwm()
    crdt.put(1, 1)
    crdt.put(2, 2)
    crdt.remove(2)
    crdt.values should ===(Map(1 -> 1))
    crdt.put(2, 2)
    crdt.remove(1)
    crdt.values should ===(Map(2 -> 2))
  }

  it should "work for multiple removes" in {
    for (i <- 1 to 10) {
      val crdt = awlwwm()
      for (j <- 1 to i) {
        crdt.put(j, j)
      }
      crdt.values should ===(genMap(1, i))
      for (j <- 1 to i) {
        crdt.remove(j)
        crdt.values should ===(genMap(1, i).removedAll(1 to j))
      }
    }
  }
}
