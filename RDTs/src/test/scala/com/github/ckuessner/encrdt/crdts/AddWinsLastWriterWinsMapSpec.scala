package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.crdts.AddWinsLastWriterWinsMap
import com.github.ckuessner.encrdt.lattices.{AddWinsMapLattice, CausalTimeTag, LastWriterWinsRegisterLattice, SemiLattice}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers._

import scala.annotation.tailrec

class AddWinsLastWriterWinsMapSpec extends AnyFlatSpec {
  type LatticeType = AddWinsMapLattice[Int, LastWriterWinsRegisterLattice[String, CausalTimeTag]]

  def merged[L: SemiLattice](a: L, b: L): L =
    SemiLattice[L].merged(a, b)

  def crdt[K, V](replicaId: String): AddWinsLastWriterWinsMap[K, V] = new AddWinsLastWriterWinsMap[K, V](replicaId)

  def crdt[K, V](lattice: AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, CausalTimeTag]],
                 replicaId: String = ""): AddWinsLastWriterWinsMap[K, V] =
    new AddWinsLastWriterWinsMap(replicaId, lattice)

  def wrapAndMerge[K, V](initialState: AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, CausalTimeTag]],
                         stateToMerge: AddWinsMapLattice[K, LastWriterWinsRegisterLattice[V, CausalTimeTag]],
                         replicaId: String = ""): AddWinsLastWriterWinsMap[K, V] = {
    val c = crdt(initialState)
    c.merge(stateToMerge)
    c
  }

  def genMap(from: Int, to: Int): Map[Int, Int] =
    (from to to).map(i => i -> i).toMap

  def genCrdt[K, V](mappings: List[(K, V)], replicaId: String): AddWinsLastWriterWinsMap[K, V] = {
    @tailrec
    def rec(mappings: List[(K, V)], crdt: AddWinsLastWriterWinsMap[K, V]): AddWinsLastWriterWinsMap[K, V] = mappings match {
      case head :: next =>
        crdt.put(head._1, head._2)
        rec(next, crdt)
      case Nil => crdt
    }

    rec(mappings, crdt(replicaId))
  }

  "put" should "add if new" in {
    val crdtA = new AddWinsLastWriterWinsMap[Int, Int]("A")
    for (i <- 0 to 10) {
      crdtA.put(i, i)
      for (j <- 0 to i) {
        crdtA.get(j) should ===(Some(j))
      }
      for (j <- i + 1 to 10) {
        crdtA.get(j) should ===(None)
      }
    }
  }

  it should "replace if exists" in {
    val crdtA = new AddWinsLastWriterWinsMap[Int, Int]("A")
    crdtA.put(1, 0)
    crdtA.put(1, 1)
    crdtA.get(1) should ===(Some(1))
    crdtA.put(0, 42)
    crdtA.get(1) should ===(Some(1))
    crdtA.get(0) should ===(Some(42))
  }

  "merge" should "be idempotent" in {
    val crdtA = new AddWinsLastWriterWinsMap[Int, Int]("A")
    crdtA.put(1, 1)
    crdtA.put(2, 2)
    val state = crdtA.state

    merged(state, state) must ===(state)

    crdtA.put(3, 3)
    val stateAfterPut3 = crdtA.state
    wrapAndMerge(state, stateAfterPut3).values should ===(genMap(1, 3))
    wrapAndMerge(merged(state, stateAfterPut3), stateAfterPut3).values should ===(genMap(1, 3))
    wrapAndMerge(stateAfterPut3, state).values should ===(genMap(1, 3))
  }

  it should "keep causal value when replacing" in {
    val crdtA = genCrdt(List(1 -> 42), "A")
    val beforeReplace = crdtA.state
    crdtA.put(1, 1)
    val afterReplace = crdtA.state
    wrapAndMerge(beforeReplace, afterReplace).values should ===(Map(1 -> 1))
  }

  it should "delete only if causal" in {
    val A = genCrdt(List(1 -> 42), "A")
    val B = crdt[Int, Int](A.state, "B")

    A.remove(1)
    B.put(2, 2)
    B.merge(A.state)
    B.values should ===(Map(2 -> 2))

    B.put(3, 3)
    B.remove(2)
    A.put(1, 1)
    A.remove(1)
    B.merge(A.state)
    B.values should ===(Map(3 -> 3))
    A.merge(B.state)
    A.values should ===(Map(3 -> 3))

    B.put(1, 3)
    A.merge(B.state)
    A.values should ===(Map(1 -> 3, 3 -> 3))
  }

  "Last write" should "win" in {
    val A = crdt[Int, Int]("A")
    val B = crdt[Int, Int]("B")

    for (_ <- 1 to 100) {
      A.put(1, 1)
      Thread.sleep(1)
      B.put(1, 2)

      crdt(merged(A.state, B.state)).values should ===(Map(1 -> 2))
      crdt(merged(B.state, A.state)).values should ===(Map(1 -> 2))
    }
  }
}
