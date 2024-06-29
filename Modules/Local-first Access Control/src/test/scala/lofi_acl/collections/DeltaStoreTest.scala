package lofi_acl.collections

import munit.FunSuite
import rdts.base.{Lattice, Uid}
import rdts.time.{ArrayRanges, Dot, Dots}

class DeltaStoreTest extends FunSuite {

  private val a = Uid("a")
  private val b = Uid("b")
  private val c = Uid("c")
  private val d = Uid("d")

  private val testStore = DeltaStore.empty[Set[Int]]()
    .addDeltaIfNew(Dot(a, 0).dots, Set(0))
    .addDeltaIfNew(Dot(a, 1).dots, Set(1))
    .addDeltaIfNew(Dot(a, 2).dots, Set(2))
    .addDeltaIfNew(Dot(a, 5).dots, Set(5))
    .addDeltaIfNew(Dot(b, 42).dots, Set(42))
    .addDeltaIfNew(Dot(c, 21).dots, Set(21))
    .addDeltaIfNew(Dot(c, 4711).dots, Set(4711))
    .addDeltaIfNew(Dot(d, 7).dots, Set(7))

  test("replacePrefixPruningDeltas and mergeIntoPrefixPruningDeltas prunes deltas") {
    val store = testStore

    val aRange = new ArrayRanges(Array(0, 6), 2)
    val bRange = new ArrayRanges(Array(0, 7), 2) // (b,42) is not included
    val cRange = new ArrayRanges(Array(4711, 4712, 10_000, 20_000), 4)
    // d is not included
    val prefixDots = Dots(Map(a -> aRange, b -> bRange, c -> cRange))

    test(store.replacePrefixPruningDeltas(prefixDots, Set.empty))
    test(store.mergeIntoPrefixPruningDeltas(prefixDots, Set.empty))

    def test(prunedStore: DeltaStore[Set[Int]]): Unit =
      assertEquals(prunedStore.readAvailableDeltas(Dot(d, 7).dots), Seq(Dot(d, 7).dots -> Set(7)))
      assertEquals(prunedStore.readAvailableDeltas(prefixDots), Seq(prefixDots -> Set.empty))
      assertEquals(prunedStore.readAvailableDeltas(Dot(a, 0).dots), Seq(prefixDots -> Set.empty))
      assertEquals(
        prunedStore.readAvailableDeltas(Dot(c, 10_022).dots.add(Dot(d, 7))),
        Seq(Dot(d, 7).dots -> Set(7), prefixDots -> Set.empty)
      )
      assertEquals(
        prunedStore.readAvailableDeltas(Dot(c, 10_022).dots.add(Dot(d, 7)).add(Dot(b, 42))).toSet,
        Set(Dot(d, 7).dots -> Set(7), prefixDots -> Set.empty, Dot(b, 42).dots -> Set(42))
      )
  }

  test("Pruning retains deltas where only some of the Dots were replaced with prefix") {
    val dotsOfDelta = Dots.single(d, 30).add(d, 45).add(a, 99)
    val delta       = Set(30, 45, 99)

    val store = testStore
      .addDeltaIfNew(dotsOfDelta, delta)

    assertEquals(
      store.replacePrefixPruningDeltas(Dots.single(d, 30).add(a, 99), Set(30, 99))
        .readAvailableDeltas(Dots.single(d, 45)),
      List(dotsOfDelta -> delta)
    )

    assertEquals(
      store.mergeIntoPrefixPruningDeltas(Dots.single(d, 30).add(a, 99), Set(30, 99))
        .readAvailableDeltas(Dots.single(d, 45)),
      List(dotsOfDelta -> delta)
    )
  }

  test("readAvailableDeltas considers both prefix and individual deltas") {
    val store = DeltaStore.empty[Set[Int]]()
      .replacePrefixPruningDeltas(Dots.single(a, 0), Set(0))
      .addDeltaIfNew(Dots.single(a, 1).add(a, 2), Set(1, 2))
      .addDeltaIfNew(Dot(b, 42).dots, Set(42))

    assertEquals(
      store.readAvailableDeltas(Dots.single(a, 0).add(a, 1).add(a, 100)).toSet, // a -> 100 is not available
      Set(
        Dot(a, 0).dots              -> Set(0), // prefix
        Dots.single(a, 1).add(a, 2) -> Set(1, 2)
      )
    )
  }

  test("addDeltaEvenIfRedundant actually adds delta if redundant") {
    val store = DeltaStore.empty[Set[Int]]()
      .replacePrefixPruningDeltas(Dots.single(a, 0).add(a, 1), Set(0, 1))
      .addDeltaIfNew(Dots.single(b, 0).add(b, 1), Set(10, 11))

    // Delta redundant with prefix
    assertEquals(
      store
        .addDeltaEvenIfRedundant(Dots.single(a, 1), Set(1))
        .readAvailableDeltas(Dots.single(a, 1)),
      List(Dots.single(a, 1) -> Set(1))
    )

    // Delta redundant with individual delta
    {
      val store2 = store.addDeltaEvenIfRedundant(Dots.single(b, 0), Set(10))
      assertEquals(
        store2.readAvailableDeltas(Dots.single(b, 0)),
        List(Dots.single(b, 0) -> Set(10))
      )
      assertEquals(
        store2
          .readAvailableDeltas(Dots.single(b, 1)),
        List(Dots.single(b, 0).add(b, 1) -> Set(10, 11))
      )
    }

    // Delta redundant with both prefix and individual delta
    {
      val store3 = store
        .addDeltaEvenIfRedundant(Dots.single(a, 1).add(b, 1), Set(1, 11))
      assertEquals(
        store3.readAvailableDeltas(Dots.single(a, 1)),
        List(Dots.single(a, 1).add(b, 1) -> Set(1, 11))
      )
      assertEquals(
        store3.readAvailableDeltas(Dots.single(b, 1)),
        List(Dots.single(a, 1).add(b, 1) -> Set(1, 11))
      )
      assertEquals(
        store3.readAvailableDeltas(Dots.single(a, 1).add(b, 1)),
        List(Dots.single(a, 1).add(b, 1) -> Set(1, 11))
      )
    }

    // Delta not redundant at all
    assertEquals(
      store
        .addDeltaEvenIfRedundant(Dots.single(a, 2), Set(2))
        .readAvailableDeltas(Dots.single(a, 2)),
      List(Dots.single(a, 2) -> Set(2))
    )
  }

  test("compactAllDeltasIntoPrefix") {
    val dottedDeltaLattice: Lattice[(Dots, Set[Int])] = Lattice.derived

    val store = testStore
      .addDeltaIfNew(Dots.single(d, 30).add(d, 45).add(a, 99), Set(30, 45, 99))

    val compacted = store.compactAllDeltasIntoPrefixSkippingDuplicates

    val mergedDeltas = store.deltas.values.reduce { (l, r) =>
      dottedDeltaLattice.merge(l, r)
    }

    assertEquals(compacted.prefixDots -> compacted.prefix, mergedDeltas)
    assert(compacted.addressableDeltas.isEmpty)
    assert(compacted.deltas.isEmpty)

    assert(store.compactAllDeltasIntoPrefixSkippingDuplicates.equals(store.compactAllDeltasIntoPrefix))
  }
}
