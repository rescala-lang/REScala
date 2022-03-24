package de.ckuessner
package encrdt.causality.impl

import encrdt.causality.impl.Defs.Time

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.language.implicitConversions

class ArrayRangesTest extends AnyFlatSpec {

  "intersect" should "work" in {
    var left  = ArrayRanges(Seq((1, 1)))
    var right = ArrayRanges(Seq((1, 1)))

    ArrayRanges.intersect(left, right) shouldEqual ArrayRanges(Seq((1, 1)))
    ArrayRanges.intersect(right, left) shouldEqual ArrayRanges(Seq((1, 1)))

    right = ArrayRanges(Seq((2, 2)))

    ArrayRanges.intersect(left, right) shouldEqual ArrayRanges.empty
    ArrayRanges.intersect(right, left) shouldEqual ArrayRanges.empty

    left = ArrayRanges.empty
    right = ArrayRanges.empty

    ArrayRanges.intersect(left, right) shouldEqual ArrayRanges.empty
    ArrayRanges.intersect(right, left) shouldEqual ArrayRanges.empty

    left = ArrayRanges(Seq((0, 10), (15, 20)))
    ArrayRanges.intersect(left, right) shouldEqual ArrayRanges.empty
    ArrayRanges.intersect(right, left) shouldEqual ArrayRanges.empty

    right = ArrayRanges(Seq((0, 10)))
    ArrayRanges.intersect(left, right) shouldEqual ArrayRanges(Seq((0, 10)))
    ArrayRanges.intersect(right, left) shouldEqual ArrayRanges(Seq((0, 10)))

    right = ArrayRanges(Seq((2, 8)))
    ArrayRanges.intersect(left, right) shouldEqual ArrayRanges(Seq((2, 8)))
    ArrayRanges.intersect(right, left) shouldEqual ArrayRanges(Seq((2, 8)))

    right = ArrayRanges(Seq((2, 3), (4, 6)))
    ArrayRanges.intersect(left, right) shouldEqual ArrayRanges(Seq((2, 3), (4, 6)))
    ArrayRanges.intersect(right, left) shouldEqual ArrayRanges(Seq((2, 3), (4, 6)))

    right = ArrayRanges(Seq((2, 3), (4, 12)))
    ArrayRanges.intersect(left, right) shouldEqual ArrayRanges(Seq((2, 3), (4, 10)))
    ArrayRanges.intersect(right, left) shouldEqual ArrayRanges(Seq((2, 3), (4, 10)))

    right = ArrayRanges(Seq((2, 3), (4, 12)))
    ArrayRanges.intersect(left, right) shouldEqual ArrayRanges(Seq((2, 3), (4, 10)))
    ArrayRanges.intersect(right, left) shouldEqual ArrayRanges(Seq((2, 3), (4, 10)))

    right = ArrayRanges(Seq((2, 3), (4, 16), (18, 22)))
    ArrayRanges.intersect(left, right) shouldEqual ArrayRanges(Seq((2, 3), (4, 10), (15, 16), (18, 20)))
    ArrayRanges.intersect(right, left) shouldEqual ArrayRanges(Seq((2, 3), (4, 10), (15, 16), (18, 20)))
  }

  private implicit def iteratorConv(range: Iterator[Int]): Iterator[Time] = range.map(i => i)

  "from" should "work" in {
    ArrayRanges.from(Seq[Time](1, 2, 3).iterator) shouldEqual ArrayRanges(Seq((1, 3)))
    ArrayRanges.from(Seq[Time](3, 2, 1).iterator) shouldEqual ArrayRanges(Seq((1, 3)))

    ArrayRanges.from(Seq[Time](1, 3).iterator) shouldEqual ArrayRanges(Seq((1, 1), (3, 3)))
    ArrayRanges.from(Seq[Time](3, 1).iterator) shouldEqual ArrayRanges(Seq((1, 1), (3, 3)))

    ArrayRanges.from(((1 to 3) ++ (5 to 8)).iterator) shouldEqual ArrayRanges(Seq((1,3), (5,8)))
    ArrayRanges.from(((5 to 8) ++ (1 to 3)).iterator) shouldEqual ArrayRanges(Seq((1,3), (5,8)))
  }

}
