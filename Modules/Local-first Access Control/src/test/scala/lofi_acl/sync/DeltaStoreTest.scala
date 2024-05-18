package lofi_acl.sync

import munit.FunSuite
import rdts.base.Uid
import rdts.time.{ArrayRanges, Dot, Dots}

class DeltaStoreTest extends FunSuite {

  private val a = Uid("a")
  private val b = Uid("b")
  private val c = Uid("c")
  private val d = Uid("d")

  test("writePrefix prunes deltas") {
    val store = DeltaStore[Set[Int]]()

    store.writeIfNotPresent(Dot(a, 0).dots, Set(0))
    store.writeIfNotPresent(Dot(a, 1).dots, Set(1))
    store.writeIfNotPresent(Dot(a, 2).dots, Set(2))
    store.writeIfNotPresent(Dot(a, 5).dots, Set(5))
    store.writeIfNotPresent(Dot(b, 42).dots, Set(42))
    store.writeIfNotPresent(Dot(c, 21).dots, Set(21))
    store.writeIfNotPresent(Dot(c, 4711).dots, Set(4711))
    store.writeIfNotPresent(Dot(d, 7).dots, Set(7))

    val aRange = new ArrayRanges(Array(0, 6), 2)
    val bRange = new ArrayRanges(Array(0, 7), 2) // (b,42) is not included
    val cRange = new ArrayRanges(Array(4711, 4712, 10_000, 20_000), 4)
    // d is not included
    val prefixDots = Dots(Map(a -> aRange, b -> bRange, c -> cRange))
    store.writePrefix(prefixDots, Set.empty)

    assertEquals(store.readAvailableDeltas(Dot(d, 7).dots), Seq(Dot(d, 7).dots -> Set(7)))
    assertEquals(store.readAvailableDeltas(prefixDots), Seq(prefixDots -> Set.empty))
    assertEquals(store.readAvailableDeltas(Dot(a, 0).dots), Seq(prefixDots -> Set.empty))
    assertEquals(
      store.readAvailableDeltas(Dot(c, 10_022).dots.add(Dot(d, 7))),
      Seq(Dot(d, 7).dots -> Set(7), prefixDots -> Set.empty)
    )
    assertEquals(
      store.readAvailableDeltas(Dot(c, 10_022).dots.add(Dot(d, 7)).add(Dot(b, 42))).toSet,
      Set(Dot(d, 7).dots -> Set(7), prefixDots -> Set.empty, Dot(b, 42).dots -> Set(42))
    )
  }

  test("write is readable without prefix") {
    val store = DeltaStore[Set[Int]]()

    store.writeIfNotPresent(Dot(a, 0).dots, Set(0))
    assertEquals(store.readAvailableDeltas(Dot(a, 0).dots), Seq(Dot(a, 0).dots -> Set(0)))

    store.writeIfNotPresent(Dot(b, 42).dots, Set(42))
    assertEquals(store.readAvailableDeltas(Dot(a, 0).dots), Seq(Dot(a, 0).dots -> Set(0)))
    assertEquals(store.readAvailableDeltas(Dot(b, 42).dots), Seq(Dot(b, 42).dots -> Set(42)))

    // Read combined
    assertEquals(
      store.readAvailableDeltas(Dot(b, 42).dots.add(Dot(a, 0))).toSet,
      Set(Dot(b, 42).dots -> Set(42), Dot(a, 0).dots -> Set(0))
    )
  }

  test("readAvailable ignores missing deltas") {
    val store = DeltaStore[Set[Int]]()

    assertEquals(store.readAvailableDeltas(Dot(a, 0).dots), Seq.empty)

    store.writeIfNotPresent(Dot(a, 0).dots, Set(0))
    assertEquals(store.readAvailableDeltas(Dot(a, 1).dots), Seq.empty)
    assertEquals(store.readAvailableDeltas(Dot(b, 0).dots), Seq.empty)

    // Only one of the two dots missing
    assertEquals(store.readAvailableDeltas(Dot(a, 0).dots.add(Dot(b, 1))), Seq(Dot(a, 0).dots -> Set(0)))

    store.writeIfNotPresent(Dot(b, 21).dots, Set(21))
    assertEquals(store.readAvailableDeltas(Dot(a, 1).dots), Seq.empty)
    assertEquals(store.readAvailableDeltas(Dot(b, 0).dots), Seq.empty)
    assertEquals(store.readAvailableDeltas(Dot(b, 20).dots), Seq.empty)

    // Works with prefix
    val aRange = new ArrayRanges(Array(0, 6), 2)
    val bRange = new ArrayRanges(Array(0, 7), 2) // (b,42) is not included
    val cRange = new ArrayRanges(Array(4711, 4712, 10_000, 20_000), 4)
    val prefixDots = Dots(Map(a -> aRange, b -> bRange, c -> cRange))
    store.writePrefix(prefixDots, Set.empty)

    assertEquals(store.readAvailableDeltas(Dot(b, 42).dots), Seq.empty)
  }

}
