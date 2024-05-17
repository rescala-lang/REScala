package test.rdts.corestructs

import org.scalacheck.Prop.*
import rdts.base.Uid
import rdts.time.{ArrayRanges, Dot, Time}
import test.rdts.DataGenerator.*

class ArrayRangesTest extends munit.ScalaCheckSuite {

  test("contains works") {
    val a = ArrayRanges(Seq[(Long, Long)]((5, 10), (20, 40)))
    List(Range(5, 10), Range(20, 40)).flatten.foreach { i => assert(a.contains(i)) }
    List(Range(10, 20), Range(40, 50)).flatten.foreach { i => assert(!a.contains(i)) }
  }

  test("merge") {

    val empty = ArrayRanges.empty

    val a = ArrayRanges(Seq[(Long, Long)]((5, 10), (20, 40)))
    val b = ArrayRanges(Seq[(Long, Long)]((10, 20)))

    assert(empty.union(b) == b)
    assert(b.union(empty) == b)

    assert(a.union(b) == ArrayRanges(Seq[(Long, Long)]((5, 40))))
  }

  def ar(times: (Time, Time)*): ArrayRanges = ArrayRanges(times)

  test("intersect") {
    var left  = ArrayRanges(Seq((1, 2)))
    var right = ArrayRanges(Seq((1, 2)))

    assertEquals((left intersect right), ArrayRanges(Seq((1, 2))))
    assertEquals((right intersect left), ArrayRanges(Seq((1, 2))))

    right = ArrayRanges(Seq((2, 3)))

    assertEquals((left intersect right), ArrayRanges.empty)
    assertEquals((right intersect left), ArrayRanges.empty)

    left = ArrayRanges.empty
    right = ArrayRanges.empty

    assertEquals((left intersect right), ArrayRanges.empty)
    assertEquals((right intersect left), ArrayRanges.empty)

    left = ArrayRanges(Seq((0, 10), (15, 20)))
    assertEquals((left intersect right), ArrayRanges.empty)
    assertEquals((right intersect left), ArrayRanges.empty)

    right = ArrayRanges(Seq((0, 10)))
    assertEquals((left intersect right), ArrayRanges(Seq((0, 10))))
    assertEquals((right intersect left), ArrayRanges(Seq((0, 10))))

    right = ArrayRanges(Seq((2, 8)))
    assertEquals((left intersect right), ArrayRanges(Seq((2, 8))))
    assertEquals((right intersect left), ArrayRanges(Seq((2, 8))))

    right = ArrayRanges(Seq((2, 3), (4, 6)))
    assertEquals((left intersect right), ArrayRanges(Seq((2, 3), (4, 6))))
    assertEquals((right intersect left), ArrayRanges(Seq((2, 3), (4, 6))))

    right = ArrayRanges(Seq((2, 3), (4, 12)))
    assertEquals((left intersect right), ArrayRanges(Seq((2, 3), (4, 10))))
    assertEquals((right intersect left), ArrayRanges(Seq((2, 3), (4, 10))))

    right = ArrayRanges(Seq((2, 3), (4, 12)))
    assertEquals((left intersect right), ArrayRanges(Seq((2, 3), (4, 10))))
    assertEquals((right intersect left), ArrayRanges(Seq((2, 3), (4, 10))))

    right = ArrayRanges(Seq((2, 3), (4, 16), (18, 22)))
    assertEquals((left intersect right), ArrayRanges(Seq((2, 3), (4, 10), (15, 16), (18, 20))))
    assertEquals((right intersect left), ArrayRanges(Seq((2, 3), (4, 10), (15, 16), (18, 20))))

    right = ArrayRanges(Seq((10, 15)))
    assertEquals((left intersect right), ArrayRanges.empty)
    assertEquals((right intersect left), ArrayRanges.empty)
  }

  private implicit def iteratorConv(range: Iterator[Int]): Iterator[Time] = range.map(i => i)

  test("from") {
    assertEquals(ArrayRanges.from(Seq[Time](1, 2, 3)), ArrayRanges(Seq((1, 4))))
    assertEquals(ArrayRanges.from(Seq[Time](3, 2, 1)), ArrayRanges(Seq((1, 4))))

    assertEquals(ArrayRanges.from(Seq[Time](1, 3)), ArrayRanges(Seq((1, 2), (3, 4))))
    assertEquals(ArrayRanges.from(Seq[Time](3, 1)), ArrayRanges(Seq((1, 2), (3, 4))))

    assertEquals(ArrayRanges.from(((1L to 3) ++ (5L to 8))), ArrayRanges(Seq((1, 4), (5, 9))))
    assertEquals(ArrayRanges.from(((5L to 8) ++ (1L to 3))), ArrayRanges(Seq((1, 4), (5, 9))))
  }

  test("duplicates") {
    assertEquals(ArrayRanges.from(Seq[Time](1, 1)), ArrayRanges(Seq((1, 2))))
    assertEquals(ArrayRanges.from(Seq[Time](1, 3, 3)), ArrayRanges(Seq((1, 2), (3, 4))))
    assertEquals(
      ArrayRanges.from(Seq[Time](1, 3, 3, 3, 4, 4, 5, 8, 8, 10)),
      ArrayRanges(Seq((1, 2), (3, 6), (8, 9), (10, 11)))
    )
  }

  test("subtract subrange") {
    assertEquals(ArrayRanges.elems(1, 2, 3) subtract ArrayRanges.elems(2), ArrayRanges.elems(1, 3))
  }

  test("subtract should result in empty range for complete overlap") {
    assertEquals((ArrayRanges(Seq((1, 10))) subtract ArrayRanges(Seq((1, 10)))), ArrayRanges.empty)
    assertEquals((ArrayRanges(Seq((1, 10))) subtract ArrayRanges(Seq((0, 10)))), ArrayRanges.empty)
    assertEquals((ArrayRanges(Seq((1, 10))) subtract ArrayRanges(Seq((0, 11)))), ArrayRanges.empty)

    assertEquals((ArrayRanges(Seq((1, 5), (6, 10))) subtract ArrayRanges(Seq((1, 5), (6, 10)))), ArrayRanges.empty)
    assertEquals((ArrayRanges(Seq((1, 5), (6, 10))) subtract ArrayRanges(Seq((1, 10)))), ArrayRanges.empty)
    assertEquals((ArrayRanges(Seq((1, 5), (6, 10))) subtract ArrayRanges(Seq((0, 10)))), ArrayRanges.empty)
    assertEquals((ArrayRanges(Seq((1, 5), (6, 10))) subtract ArrayRanges(Seq((0, 11)))), ArrayRanges.empty)
  }

  test("overlap on left but not right") {
    assertEquals((ArrayRanges(Seq((1, 10))) subtract ArrayRanges(Seq((0, 5)))), ArrayRanges(Seq((5, 10))))
    assertEquals(
      (ArrayRanges(Seq((1, 10), (11, 20))) subtract ArrayRanges(Seq((0, 5)))),
      ArrayRanges(Seq((5, 10), (11, 20)))
    )
    assertEquals(
      (ArrayRanges(Seq((1, 10), (11, 20))) subtract ArrayRanges(Seq((11, 19)))),
      ArrayRanges(Seq((1, 10), (19, 20)))
    )
    assertEquals(
      (ArrayRanges(Seq((1, 10), (11, 20))) subtract ArrayRanges(Seq((10, 19)))),
      ArrayRanges(Seq((1, 10), (19, 20)))
    )
    assertEquals(
      (ArrayRanges(Seq((1, 10), (11, 20), (21, 22))) subtract ArrayRanges(Seq((11, 19)))),
      ArrayRanges(Seq((1, 10), (19, 20), (21, 22)))
    )
  }

  test("overlap on right but not left") {
    assertEquals((ArrayRanges(Seq((1, 10))) subtract ArrayRanges(Seq((5, 10)))), ArrayRanges(Seq((1, 5))))
    assertEquals((ArrayRanges(Seq((1, 10))) subtract ArrayRanges(Seq((5, 12)))), ArrayRanges(Seq((1, 5))))
    assertEquals(
      (ArrayRanges(Seq((1, 10), (11, 20))) subtract ArrayRanges(Seq((0, 5)))),
      ArrayRanges(Seq((5, 10), (11, 20)))
    )
    assertEquals(
      (ArrayRanges(Seq((1, 10), (11, 20))) subtract ArrayRanges(Seq((11, 19)))),
      ArrayRanges(Seq((1, 10), (19, 20)))
    )
    assertEquals(
      (ArrayRanges(Seq((1, 10), (11, 20))) subtract ArrayRanges(Seq((10, 19)))),
      ArrayRanges(Seq((1, 10), (19, 20)))
    )
    assertEquals(
      (ArrayRanges(Seq((1, 10), (11, 20), (21, 22))) subtract ArrayRanges(Seq((11, 19)))),
      ArrayRanges(Seq((1, 10), (19, 20), (21, 22)))
    )
  }

  test("for combinations of overlap") {
    assertEquals(
      (ArrayRanges(Seq((0, 2), (5, 10), (11, 20))) subtract ArrayRanges(Seq((0, 5)))),
      ArrayRanges(Seq((5, 10), (11, 20)))
    )
    assertEquals(
      (ArrayRanges(Seq((0, 2), (5, 10), (11, 20))) subtract ArrayRanges(Seq((0, 6)))),
      ArrayRanges(Seq((6, 10), (11, 20)))
    )
    assertEquals(
      (ArrayRanges(Seq((0, 2), (5, 10), (11, 20))) subtract ArrayRanges(Seq((6, 19)))),
      ArrayRanges(Seq((0, 2), (5, 6), (19, 20)))
    )
    assertEquals(
      (ArrayRanges(Seq((0, 2), (5, 10), (11, 20))) subtract ArrayRanges(Seq((6, 22)))),
      ArrayRanges(Seq((0, 2), (5, 6)))
    )
  }

  test("work with no overlap") {
    var left = ArrayRanges(Seq((5, 10), (11, 20)))
    assertEquals((left subtract ArrayRanges(Seq((0, 5)))), left)
    assertEquals((left subtract ArrayRanges(Seq((0, 4)))), left)
    assertEquals((left subtract ArrayRanges(Seq((20, 25)))), left)

    left = ArrayRanges(Seq((0, 2), (5, 10), (11, 20)))
    assertEquals((left subtract ArrayRanges(Seq((2, 5)))), left)
    assertEquals((left subtract ArrayRanges(Seq((2, 3)))), left)
    assertEquals((left subtract ArrayRanges(Seq((3, 5)))), left)
    assertEquals((left subtract ArrayRanges(Seq((10, 11)))), left)

  }

  test("work if left is empty") {
    val left  = ArrayRanges.empty
    val right = ArrayRanges(Seq((1, 5)))
    assertEquals((left subtract right), ArrayRanges.empty)
  }

  test("work if right is empty") {
    var left  = ArrayRanges(Seq((1, 5)))
    val right = ArrayRanges.empty
    assertEquals((left subtract right), left)

    left = ArrayRanges(Seq((0, 20), (25, 30)))
    assertEquals((left subtract right), left)
  }

  test("work if both are empty") {
    assertEquals((ArrayRanges.empty subtract ArrayRanges.empty), ArrayRanges.empty)
  }

  test("<= should work for singles on left and right") {
    assert(ArrayRanges(Seq((1, 2))) <= ArrayRanges(Seq((1, 2))))
    assert(ArrayRanges(Seq((1, 2))) <= ArrayRanges(Seq((0, 2))))
    assert(ArrayRanges(Seq((1, 2))) <= ArrayRanges(Seq((1, 3))))
    assert(ArrayRanges(Seq((1, 2))) <= ArrayRanges(Seq((0, 3))))

    assert(!(ArrayRanges(Seq((0, 2))) <= ArrayRanges(Seq((1, 2)))))
    assert(!(ArrayRanges(Seq((1, 3))) <= ArrayRanges(Seq((1, 2)))))
    assert(!(ArrayRanges(Seq((0, 2))) <= ArrayRanges(Seq((1, 2)))))
    assert(!(ArrayRanges(Seq((0, 3))) <= ArrayRanges(Seq((1, 2)))))
  }

  test("work for empty") {
    assert(!(ArrayRanges(Seq((1, 2))) <= ArrayRanges.empty))
    assert(!(ArrayRanges(Seq((1, 2), (4, 5))) <= ArrayRanges.empty))

    assert(ArrayRanges.empty <= ArrayRanges.empty)
    assert(ArrayRanges.empty <= ArrayRanges(Seq((1, 2))))
  }

  test("work for longer ranges") {
    assert(ar((1, 2), (3, 5)) <= ar((1, 5)))
    assert(ar((1, 2), (3, 5)) <= ar((0, 5)))
    assert(ar((1, 2), (3, 5)) <= ar((1, 6)))
    assert(ar((1, 2), (3, 5)) <= ar((0, 6)))

    assert(ar((1, 2)) <= ar((1, 2), (3, 5), (6, 10)))
    assert(ar((4, 5)) <= ar((1, 2), (3, 5), (6, 10)))
    assert(ar((6, 8)) <= ar((1, 2), (3, 5), (6, 10)))

    assert(!(ar((11, 20)) <= ar((1, 2), (3, 5), (6, 10))))

    assert(!(ar((1, 2), (4, 5)) <= ar((1, 2))))
    assert(!(ar((1, 2), (4, 5)) <= ar((1, 2), (5, 6))))
    assert((ar((1, 2), (4, 5)) <= ar((1, 5), (10, 11))))
    assert(!(ar((1, 2), (4, 5)) <= ar((1, 4), (10, 11))))
  }

  property("same as set") {
    forAll { (left: SmallTimeSet, right: SmallTimeSet) =>
      val leftSet  = left.s
      val rightSet = right.s

      val lr = ArrayRanges.from(leftSet)
      val rr = ArrayRanges.from(rightSet)

      assertEquals(lr union rr, ArrayRanges.from((leftSet union rightSet)))
      assertEquals(lr subtract rr, ArrayRanges.from((leftSet diff rightSet)))

      if lr <= rr then
        assertEquals(leftSet.intersect(rightSet), leftSet, s"left: $left\nright: $right")
        assertEquals(leftSet.union(rightSet), rightSet)

      assertEquals(lr.disjunct(rr), leftSet.intersect(rightSet).isEmpty)
    }
  }

  test("head throws NoSuchElementException when empty") {
    intercept[NoSuchElementException](new ArrayRanges(Array.empty[Time], 0).head)
    intercept[NoSuchElementException](new ArrayRanges(Array[Time](1, 2, 6, 9), 0).head)
  }

  test("head works with nonempty ranges") {
    assertEquals(new ArrayRanges(Array(1, 3, 6, 9), 4).head, 1.asInstanceOf[Time])
    assertEquals(new ArrayRanges(Array(7, 9), 2).head, 7.asInstanceOf[Time])
    assertEquals(new ArrayRanges(Array(7, 9, 10, 11), 2).head, 7.asInstanceOf[Time])
  }

}
