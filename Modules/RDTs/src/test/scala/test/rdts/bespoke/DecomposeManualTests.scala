package test.rdts.bespoke

import rdts.base.Uid.asId
import rdts.base.{Bottom, Lattice}
import rdts.datatypes.contextual.{EnableWinsFlag, MultiVersionRegister}
import rdts.datatypes.{GrowOnlyCounter, GrowOnlyMap, GrowOnlySet, LastWriterWins, PosNegCounter}
import rdts.dotted.{Dotted, HasDots}
import rdts.syntax.LocalUid
import rdts.time.{Dot, Dots}
import test.rdts.UtilHacks.*
import rdts.datatypes.GrowOnlyMap.mutateKeyNamedCtx


class DecomposeManualTests extends munit.ScalaCheckSuite {

  val r1: LocalUid = "r1".asId
  val r2: LocalUid = "r2".asId

  test("GrowOnlyCounter decomposition") {
    val empty: GrowOnlyCounter = Bottom[GrowOnlyCounter].empty

    val delta_1: GrowOnlyCounter = empty.inc()(using r1)
    assertEquals(delta_1.value, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: GrowOnlyCounter = empty.inc()(using r2)
    assertEquals(delta_2.value, 1)

    val merged: GrowOnlyCounter = Lattice[GrowOnlyCounter].merge(delta_1, delta_2)
    assertEquals(merged.value, 2)

    val decomposed: Seq[GrowOnlyCounter] = Lattice[GrowOnlyCounter].decompose(merged).toSeq
    // GrowOnlyCounter decomposes into every increment
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).value, 1)
    assertEquals(decomposed(1).value, 1)
  }

  test("PosNegCounter decomposition") {
    val empty: PosNegCounter = Bottom[PosNegCounter].empty

    val delta_1: PosNegCounter = empty.inc()(using r1)
    assertEquals(delta_1.value, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: PosNegCounter = empty.dec()(using r2)
    assertEquals(delta_2.value, -1)

    val merged: PosNegCounter = Lattice[PosNegCounter].merge(delta_1, delta_2)
    assertEquals(merged.value, 0)

    val decomposed: Seq[PosNegCounter] = Lattice[PosNegCounter].decompose(merged).toSeq.sortBy(_.value)
    // GrowOnlyCounter decomposes into every increment & decrement
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).value, -1)
    assertEquals(decomposed(1).value, 1)
  }

  test("Dotted[EnableWinsFlag] decomposition") {
    val emptyEWFlag: Dotted[EnableWinsFlag] = Dotted(Bottom[EnableWinsFlag].empty)
    assertEquals(emptyEWFlag.context, Dots.empty)

    val delta_1: Dotted[EnableWinsFlag] = emptyEWFlag.enable(using r1)()
    assertEquals(delta_1.context.internal.size, 1)
    assertEquals(delta_1.context.max(r1.uid), Some(Dot(r1.uid, 0)))
    assertEquals(delta_1.data.read, true)

    // delta_1 and delta_2 are in parallel

    val delta_2: Dotted[EnableWinsFlag] = emptyEWFlag.disable()
    assertEquals(delta_2.context.internal, Map.empty)
    assertEquals(delta_2.data.read, false)

    val merged: Dotted[EnableWinsFlag] = Lattice[Dotted[EnableWinsFlag]].merge(delta_1, delta_2)
    assertEquals(merged.context.internal.size, 1)
    assertEquals(merged.context.max(r1.uid), Some(Dot(r1.uid, 0)))
    assertEquals(merged.data.read, true)

    val decomposed: Seq[Dotted[EnableWinsFlag]] =
      Lattice[Dotted[EnableWinsFlag]].decompose(merged).toSeq.sortBy(_.data.inner.internal.keys.headOption)
    // EnableWinsFlag does not decompose, only returns the value.
    // Dotted decomposes context and value. As context is completely covered by EnableWinsFlag, no additional entry for context.
    assertEquals(decomposed.size, 1)

    assertEquals(decomposed(0).data.read, true)
    assertEquals(decomposed(0).data.dots, Dots.single(Dot(r1.uid, 0)))
    assertEquals(decomposed(0).context.toSet, Set(Dot(r1.uid, 0)))
  }

  test("Dotted[EnableWinsFlag] context decomposition") {
    val emptyEWFlag: Dotted[EnableWinsFlag] = Dotted(Bottom[EnableWinsFlag].empty)
    assertEquals(emptyEWFlag.context, Dots.empty)

    // The first enable creates a dot, first disable keep it. Second enable creates a new dot.
    // At the end, both dots are in the context, but only the last one is contained within.

    val delta_1: Dotted[EnableWinsFlag] = emptyEWFlag.enable(using r1)()
    assertEquals(delta_1.context.internal.size, 1)
    assertEquals(delta_1.context.max(r1.uid), Some(Dot(r1.uid, 0)))
    assertEquals(delta_1.data.read, true)
    assertEquals(delta_1.data.dots, Dots.single(Dot(r1.uid, 0)))

    val val_1: Dotted[EnableWinsFlag] = delta_1
    assertEquals(val_1.data.read, true)

    val delta_2: Dotted[EnableWinsFlag] = val_1.disable()
    assertEquals(delta_2.context.internal.size, 1)
    assertEquals(delta_2.context.max(r1.uid), Some(Dot(r1.uid, 0)))
    assertEquals(delta_2.data.read, false)
    assertEquals(delta_2.data.dots, Dots.empty)

    val val_2: Dotted[EnableWinsFlag] = val_1 merge delta_2
    assertEquals(val_2.data.read, false)

    val delta_3: Dotted[EnableWinsFlag] = val_2.enable(using r2)()
    assertEquals(delta_3.context.internal.size, 1)
    assertEquals(delta_3.context.toSet, Set(Dot(r2.uid, 0))) // it's a delta - r1 dot not here, only r2
    assertEquals(delta_3.data.read, true)
    assertEquals(delta_3.data.dots, Dots.single(Dot(r2.uid, 0)))

    val val_3: Dotted[EnableWinsFlag] = val_2 merge delta_3
    assertEquals(val_3.data.read, true)

    val decomposed: Seq[Dotted[EnableWinsFlag]] =
      Lattice[Dotted[EnableWinsFlag]].decompose(val_3).toSeq.sortBy(_.data.inner.internal.keys.headOption)
    // Dotted decomposes context and value - one entry for EnableWinsFlag with their Dot and one entry with remaining context
    assertEquals(decomposed.size, 2)

    assertEquals(decomposed(0).data, EnableWinsFlag.empty)
    assertEquals(decomposed(0).data.dots, Dots.empty)
    assertEquals(decomposed(0).context.toSet, Set(Dot(r1.uid, 0)))

    assertEquals(decomposed(1).data.read, true)
    assertEquals(decomposed(1).data.dots, Dots.single(Dot(r2.uid, 0)))
    assertEquals(decomposed(1).context.toSet, Set(Dot(r2.uid, 0)))
  }

  test("Dotted[MultiVersionRegister[Int]] decomposition") {

    val empty: Dotted[MultiVersionRegister[Int]] = Dotted(Bottom[MultiVersionRegister[Int]].empty)

    val delta_1: Dotted[MultiVersionRegister[Int]] = empty.write(using r1)(1)
    assertEquals(delta_1.read, Set(1))

    // delta_1 and delta_2 are in parallel

    val delta_2: Dotted[MultiVersionRegister[Int]] = empty.write(using r2)(2)
    assertEquals(delta_2.read, Set(2))

    val merged: Dotted[MultiVersionRegister[Int]] = Lattice[Dotted[MultiVersionRegister[Int]]].merge(delta_1, delta_2)
    assertEquals(merged.read, Set(1, 2))

    val decomposed: Seq[Dotted[MultiVersionRegister[Int]]] =
      Lattice[Dotted[MultiVersionRegister[Int]]].decompose(merged).toSeq.sortBy(_.data.read.headOption)
    // MultiVersionRegister decomposes every version
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).read, Set(1))
    assertEquals(decomposed(1).read, Set(2))
  }

  test("Dotted[LastWriterWins[Int]] decomposition") {
    given bottomInt: Bottom[Int] with {
      override def empty: Int = 0
    }

    val empty: Dotted[LastWriterWins[Int]] = Bottom[Dotted[LastWriterWins[Int]]].empty

    val delta_1: Dotted[LastWriterWins[Int]] = empty.write(1)
    assertEquals(delta_1.read, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: Dotted[LastWriterWins[Int]] = empty.write(2)
    assertEquals(delta_2.read, 2)

    val merged: Dotted[LastWriterWins[Int]] = Lattice[Dotted[LastWriterWins[Int]]].merge(delta_1, delta_2)
    assertEquals(merged.read, 2)

    val decomposed: Seq[Dotted[LastWriterWins[Int]]] = Lattice[Dotted[LastWriterWins[Int]]].decompose(merged).toSeq
    // LastWriterWins does not decompose, only returns the value.
    // Dotted decomposes context and value, but as LWW is not contextual, context is empty and not decomposed.
    assertEquals(decomposed.size, 1)
    assertEquals(decomposed.head.read, 2)
  }

  test("GrowOnlySet[Int] decomposition") {
    import GrowOnlySet.given

    val empty: GrowOnlySet[Int] = Bottom[GrowOnlySet[Int]].empty

    val delta_1: GrowOnlySet[Int] = empty.insert(1)
    assertEquals(delta_1.elements, Set(1))

    // delta_1 and delta_2 are in parallel

    val delta_2: GrowOnlySet[Int] = empty.insert(2)
    assertEquals(delta_2.elements, Set(2))

    val merged: GrowOnlySet[Int] = Lattice[GrowOnlySet[Int]].merge(delta_1, delta_2)
    assertEquals(merged.elements, Set(1, 2))

    val decomposed: Seq[GrowOnlySet[Int]] =
      Lattice[GrowOnlySet[Int]].decompose(merged).toSeq.sortBy(_.elements.headOption)
    // GrowOnlySet decomposes every entry
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).elements, Set(1))
    assertEquals(decomposed(1).elements, Set(2))
  }

  test("Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] decomposition") {
    import GrowOnlyMap.given

    given stringOrdering: Ordering[String] = scala.math.Ordering.String

    given stringLattice: Lattice[String] = Lattice.fromOrdering(using stringOrdering)

    given HasDots[String] = HasDots.noDots

    given bottomString: Bottom[String] with {
      override def empty: String = ""
    }

    val emptyMap: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] =
      Dotted(Bottom[GrowOnlyMap[Int, LastWriterWins[String]]].empty)
    assertEquals(emptyMap.context.internal, Map.empty)

    val k1: Int                                                   = 1
    val v1: String                                                = "one"
    val e1                                                        = LastWriterWins.now("")
    val delta_1: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] = emptyMap.mod(_.mutateKeyNamedCtx(k1, e1)(_.write(v1)))
    assertEquals(delta_1.context.internal, Map.empty)
    assertEquals(delta_1.data.keySet, Set(1))
    assertEquals(delta_1.data.get(1).map(_.payload), Some("one"))

    // delta_1 and delta_2 are in parallel

    val k2: Int                                                   = 2
    val v2: String                                                = "two"
    val e2                                                        = LastWriterWins.now("")
    val delta_2: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] = emptyMap.mod(_.mutateKeyNamedCtx(k2, e2)(_.write(v2)))
    assertEquals(delta_2.context.internal, Map.empty)
    assertEquals(delta_2.data.keySet, Set(2))
    assertEquals(delta_2.data.get(2).map(_.payload), Some("two"))

    val merged: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] =
      Lattice[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]].merge(delta_1, delta_2)
    assertEquals(merged.context.internal, Map.empty)
    assertEquals(merged.data.keySet, Set(1, 2))
    assertEquals(merged.data.get(1).map(_.payload), Some("one"))
    assertEquals(merged.data.get(2).map(_.payload), Some("two"))

    val decomposed: Seq[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]] =
      Lattice[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]].decompose(merged).toSeq.sortBy(_.data.keys.headOption)
    // GrowOnlyMap decomposes every entry.
    // LastWriterWins does not decompose, only returns the value.
    // Dotted decomposes context and value, but as LWW is not contextual, the context is empty and there is no additional entry for it.
    assertEquals(decomposed.size, 2)

    assertEquals(decomposed(0).context.internal, Map.empty)
    assertEquals(decomposed(0).data.get(1).map(_.payload), Some("one"))

    assertEquals(decomposed(1).context.internal, Map.empty)
    assertEquals(decomposed(1).data.get(2).map(_.payload), Some("two"))
  }

  test("Dotted[GrowOnlyMap[Int, EnableWinsFlag]] decomposition") {
    import GrowOnlyMap.given

    val emptyMap: Dotted[GrowOnlyMap[Int, EnableWinsFlag]] = Dotted(Bottom[GrowOnlyMap[Int, EnableWinsFlag]].empty)

    val k1: Int                                           = 1
    val e1                                                = EnableWinsFlag.empty
    val delta_1: Dotted[GrowOnlyMap[Int, EnableWinsFlag]] = emptyMap.mod(_.mutateKeyNamedCtx(k1, e1)(_.enable(using r1)()))
    assertEquals(delta_1.context.internal.size, 1)
    assertEquals(delta_1.context.max(r1.uid), Some(Dot(r1.uid, 0)))
    assertEquals(delta_1.data.keySet, Set(1))
    assertEquals(delta_1.data.get(1).map(_.read), Some(true))

    // delta_1 and delta_2 are in parallel

    val k2: Int                                           = 2
    val e2                                                = EnableWinsFlag.empty
    val delta_2: Dotted[GrowOnlyMap[Int, EnableWinsFlag]] = emptyMap.mod(_.mutateKeyNamedCtx(k2, e2)(_.enable(using r2)()))
    assertEquals(delta_2.context.internal.size, 1)
    assertEquals(delta_2.context.max(r2.uid), Some(Dot(r2.uid, 0)))
    assertEquals(delta_2.data.keySet, Set(2))
    assertEquals(delta_2.data.get(2).map(_.read), Some(true))

    val merged: Dotted[GrowOnlyMap[Int, EnableWinsFlag]] =
      Lattice[Dotted[GrowOnlyMap[Int, EnableWinsFlag]]].merge(delta_1, delta_2)
    assertEquals(merged.context.internal.size, 2)
    assertEquals(merged.context.max(r1.uid), Some(Dot(r1.uid, 0)))
    assertEquals(merged.context.max(r2.uid), Some(Dot(r2.uid, 0)))
    assertEquals(merged.data.keySet, Set(1, 2))
    assertEquals(merged.data.get(1).map(_.read), Some(true))
    assertEquals(merged.data.get(2).map(_.read), Some(true))

    val decomposed: Seq[Dotted[GrowOnlyMap[Int, EnableWinsFlag]]] =
      Lattice[Dotted[GrowOnlyMap[Int, EnableWinsFlag]]].decompose(merged).toSeq.sortBy(_.data.keys.headOption)
    // GrowOnlyMap decomposes every entry and the context.
    // EnableWinsFlag is contextual. Every entry has its own required context.
    // The complete context is covered by the entries.
    assertEquals(decomposed.size, 2)

    assertEquals(decomposed(0), delta_1) // context contains only r1, delta_2 is irrelevant for the first entry
    assertEquals(decomposed(1), delta_2) // context contains only r2, delta_1 is irrelevant for the second entry
  }

}
