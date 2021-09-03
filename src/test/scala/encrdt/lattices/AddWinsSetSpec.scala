package de.ckuessner
package encrdt.lattices

import encrdt.lattices.interfaces.SemiLattice

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class AddWinsSetSpec extends AnyFlatSpec {
  def merged[T](left: AddWinsSetLattice[T], right: AddWinsSetLattice[T]): AddWinsSetLattice[T] =
    SemiLattice[AddWinsSetLattice[T]].merged(left, right)

  "An AddWinsSet" should "merge when empty" in {
    val left = AddWinsSetLattice[Int]()

    merged(left, left) should ===(left)
  }

  it should "add element on add" in {
    val set = AddWinsSetLattice[Int]()
      .added(1, "A")
    set.values should contain(1)

    set.added(2, "B").values should contain(2)
    set.added(2, "B").values should contain(1)
  }

  it should "remove only removed element on remove" in {
    val set = AddWinsSetLattice[Int]().added(1, "A")
    set.removed(1).values should not.contain(1)
    set.removed(1).values should be(empty)

    val set2 = set.added(2, "A").added(3, "B")
    set2.removed(3).values() should not contain (3)
    set2.removed(3).values() should contain(1)
    set2.removed(3).values() should contain(2)

    set2.removed(2).values() should not contain (2)
    set2.removed(2).values() should contain(1)
    set2.removed(2).values() should contain(3)
  }

  it should "have symmetric merge" in {
    var left = AddWinsSetLattice[Int]().added(1, "A")
    var right = left
    left = left.removed(1)
    right = right.added(1, "B")

    merged(left, right) should ===(merged(right, left))
  }

  it should "prefer add over remove if parallel" in {
    var left = AddWinsSetLattice[Int]().added(1, "A")
    var right = left
    left = left.removed(1)
    right = right.added(1, "B")

    val m1 = merged(left, right)
    m1.values() should contain(1)
  }

  it should "remove element if removal is causal" in {
    var left = AddWinsSetLattice[Int]().added(1, "A")
    var right = left

    right = right.removed(1)
    right.values should not contain 1
    left = merged(left, right)
    left.values should not contain 1
  }

  it should "have idempotent merge" in {
    var left = AddWinsSetLattice[Int]().added(1, "A")
    var right = left
    left = left.removed(1)
    right = right.added(1, "B")

    val m1 = merged(left, right)
    merged(m1, left) should ===(m1)
    merged(left, m1) should ===(m1)
    merged(m1, right) should ===(m1)
    merged(right, m1) should ===(m1)
    merged(m1, m1) should ===(m1)
  }
}
