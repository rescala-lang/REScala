package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.lattices.{AddWinsSetLattice, SemiLattice}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class AddWinsSetSpec extends AnyFlatSpec {
  def merged[T](left: AddWinsSetLattice[T], right: AddWinsSetLattice[T]): AddWinsSetLattice[T] =
    SemiLattice[AddWinsSetLattice[T]].merged(left, right)

  "An AddWinsSet Lattice" should "merge when empty" in {
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
    set2.removed(3).values should not contain (3)
    set2.removed(3).values should contain(1)
    set2.removed(3).values should contain(2)

    set2.removed(2).values should not contain (2)
    set2.removed(2).values should contain(1)
    set2.removed(2).values should contain(3)
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
    m1.values should contain(1)
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

  it should "merge parallel add of different elements" in {
    val left = AddWinsSetLattice[Int]().added(1, "A")
    val right = AddWinsSetLattice[Int]().added(2, "B")
    merged(left, right).values should ===(Set(1,2))
  }

  it should "merge parallel add of same elements" in {
    val left = AddWinsSetLattice[Int]().added(1, "A")
    val right = AddWinsSetLattice[Int]().added(1, "B")
    merged(left, right).values should ===(Set(1))
  }

  it should "keep value if added twice" in {
    val left = AddWinsSetLattice[Int]().added(1, "A")
    left.added(1, "A").values should ===(Set(1))
    left.added(1, "B").values should ===(Set(1))
  }

  it should "keep values in merge if added twice" in {
    val left = AddWinsSetLattice[Int]().added(1, "A")
    val stateAfterSecondAdd = left.added(1, "A")
    merged(left, stateAfterSecondAdd).values should ===(Set(1))
  }

  it should "work with dropped states" in {
    val first = AddWinsSetLattice[Int]().added(1, "A")
    val second = first.removed(1)

    val thirdA = second.added(2, "A").removed(2)
    val fourthA = thirdA.added(1, "A")

    val thirdB = second.added(2, "B")

    merged(first, thirdB).values should ===(Set(2))
    merged(thirdA, first).values should ===(Set())
    merged(thirdB, first).values should ===(Set(2))
    merged(thirdA, thirdB).values should ===(Set(2))
    merged(first, fourthA).values should ===(Set(1))
    merged(second, fourthA).values should ===(Set(1))
    merged(fourthA, second).values should ===(Set(1))
    merged(merged(thirdB, thirdA), fourthA).values should ===(Set(1,2))
    merged(fourthA, thirdB).values should ===(Set(1,2))
  }
}
