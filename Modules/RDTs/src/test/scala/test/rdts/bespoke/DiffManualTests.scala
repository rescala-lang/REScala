package test.rdts.bespoke

import rdts.base.LocalUid.asId
import rdts.base.{Bottom, Decompose, Lattice, LocalUid}
import rdts.datatypes.GrowOnlySet.{elements, insert}
import rdts.datatypes.contextual.{EnableWinsFlag, MultiVersionRegister}
import rdts.datatypes.{GrowOnlyCounter, GrowOnlyMap, GrowOnlySet, LastWriterWins, PosNegCounter}
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{Dot, Dots}
import test.rdts.UtilHacks.*

class DiffManualTests extends munit.ScalaCheckSuite {

  val r1: LocalUid = "r1".asId
  val r2: LocalUid = "r2".asId

  test("GrowOnlyCounter diff") {
    val empty: GrowOnlyCounter = Bottom[GrowOnlyCounter].empty

    val delta_1: GrowOnlyCounter = empty.inc()(using r1)
    assertEquals(delta_1.value, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: GrowOnlyCounter = empty.inc()(using r2)
    assertEquals(delta_2.value, 1)

    val merged: GrowOnlyCounter = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.value, 2)

    val delta_1_diff_delta_2: Option[GrowOnlyCounter] = Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[GrowOnlyCounter] = Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[GrowOnlyCounter] = Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[GrowOnlyCounter] = Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("PosNegCounter diff") {
    val empty: PosNegCounter = Bottom[PosNegCounter].empty

    val delta_1: PosNegCounter = empty.inc()(using r1)
    assertEquals(delta_1.value, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: PosNegCounter = empty.dec()(using r2)
    assertEquals(delta_2.value, -1)

    val merged: PosNegCounter = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.value, 0)

    val delta_1_diff_delta_2: Option[PosNegCounter] = Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[PosNegCounter] = Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[PosNegCounter] = Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[PosNegCounter] = Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("Dotted[EnableWinsFlag] diff") {
    val emptyEWFlag: Dotted[EnableWinsFlag] = Dotted(Bottom[EnableWinsFlag].empty)
    assertEquals(emptyEWFlag.context, Dots.empty)

    val delta_1: Dotted[EnableWinsFlag] = emptyEWFlag.mod(_.enable(using r1)())
    assertEquals(delta_1.context.internal.size, 1)
    assertEquals(delta_1.context.max(r1.uid), Some(Dot(r1.uid, 0)))
    assertEquals(delta_1.data.read, true)

    // delta_1 and delta_2 are in parallel

    val delta_2: Dotted[EnableWinsFlag] = emptyEWFlag.mod(_.disable())
    assertEquals(delta_2.context.internal, Map.empty)
    assertEquals(delta_2.data.read, false)

    val merged: Dotted[EnableWinsFlag] = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.context.internal.size, 1)
    assertEquals(merged.context.max(r1.uid), Some(Dot(r1.uid, 0)))
    assertEquals(merged.data.read, true)

    val delta_1_diff_delta_2: Option[Dotted[EnableWinsFlag]] = Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, false, "delta_1 wins - delta_2 is obsolete")

    val delta_2_diff_delta_1: Option[Dotted[EnableWinsFlag]] = Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[Dotted[EnableWinsFlag]] = Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[Dotted[EnableWinsFlag]] = Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("Dotted[MultiVersionRegister[Int]] diff") {

    val empty: Dotted[MultiVersionRegister[Int]] = Dotted(Bottom[MultiVersionRegister[Int]].empty)

    val delta_1: Dotted[MultiVersionRegister[Int]] = empty.mod(_.write(using r1)(1))
    assertEquals(delta_1.data.read, Set(1))

    // delta_1 and delta_2 are in parallel

    val delta_2: Dotted[MultiVersionRegister[Int]] = empty.mod(_.write(using r2)(2))
    assertEquals(delta_2.data.read, Set(2))

    val merged: Dotted[MultiVersionRegister[Int]] = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.data.read, Set(1, 2))

    val delta_1_diff_delta_2: Option[Dotted[MultiVersionRegister[Int]]] =
      Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[Dotted[MultiVersionRegister[Int]]] =
      Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[Dotted[MultiVersionRegister[Int]]] =
      Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[Dotted[MultiVersionRegister[Int]]] =
      Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("Dotted[LastWriterWins[Int]] diff") {
    given bottomInt: Bottom[Int] with {
      override def empty: Int = 0
    }

    val empty: Dotted[LastWriterWins[Int]] = Bottom[Dotted[LastWriterWins[Int]]].empty

    val delta_1: Dotted[LastWriterWins[Int]] = empty.write(1)
    assertEquals(delta_1.data.read, 1)

    // delta_1 and delta_2 are in parallel

    val delta_2: Dotted[LastWriterWins[Int]] = empty.write(2)
    assertEquals(delta_2.data.read, 2)

    val merged: Dotted[LastWriterWins[Int]] = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.data.read, 2)

    given Decompose[LastWriterWins[Int]] = Decompose.atomic

    val delta_1_diff_delta_2: Option[Dotted[LastWriterWins[Int]]] =
      Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[Dotted[LastWriterWins[Int]]] =
      Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, false, "delta_1 happened before delta_2 - delta_1 is obsolete")

    val merged_diff_delta_1: Option[Dotted[LastWriterWins[Int]]] =
      Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[Dotted[LastWriterWins[Int]]] =
      Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("GrowOnlySet[Int] diff") {
    import GrowOnlySet.given

    val empty: GrowOnlySet[Int] = Bottom[GrowOnlySet[Int]].empty

    val delta_1: GrowOnlySet[Int] = empty.insert(1)
    assertEquals(delta_1.elements, Set(1))

    // delta_1 and delta_2 are in parallel

    val delta_2: GrowOnlySet[Int] = empty.insert(2)
    assertEquals(delta_2.elements, Set(2))

    val merged: GrowOnlySet[Int] = Lattice.merge(delta_1, delta_2)
    assertEquals(merged.elements, Set(1, 2))

    val delta_1_diff_delta_2: Option[GrowOnlySet[Int]] = Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[GrowOnlySet[Int]] = Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[GrowOnlySet[Int]] = Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[GrowOnlySet[Int]] = Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] diff") {
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

    val k1: Int    = 1
    val v1: String = "one"
    val e1         = LastWriterWins.now("")
    val delta_1: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] =
      emptyMap.mod(_.mutateKeyNamedCtx(k1, e1)(_.write(v1)))
    assertEquals(delta_1.context.internal, Map.empty)
    assertEquals(delta_1.data.keySet, Set(1))
    assertEquals(delta_1.data.get(1).map(_.payload), Some("one"))

    // delta_1 and delta_2 are in parallel

    val k2: Int    = 2
    val v2: String = "two"
    val e2         = LastWriterWins.now("")
    val delta_2: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] =
      emptyMap.mod(_.mutateKeyNamedCtx(k2, e2)(_.write(v2)))
    assertEquals(delta_2.context.internal, Map.empty)
    assertEquals(delta_2.data.keySet, Set(2))
    assertEquals(delta_2.data.get(2).map(_.payload), Some("two"))

    val merged: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] =
      Lattice.merge(delta_1, delta_2)
    assertEquals(merged.context.internal, Map.empty)
    assertEquals(merged.data.keySet, Set(1, 2))
    assertEquals(merged.data.get(1).map(_.payload), Some("one"))
    assertEquals(merged.data.get(2).map(_.payload), Some("two"))

    given Decompose[LastWriterWins[String]] = Decompose.atomic
    val delta_1_diff_delta_2: Option[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]] =
      Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]] =
      Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]] =
      Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]] =
      Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

  test("Dotted[GrowOnlyMap[Int, EnableWinsFlag]] diff") {
    import GrowOnlyMap.given

    val emptyMap: Dotted[GrowOnlyMap[Int, EnableWinsFlag]] = Dotted(Bottom[GrowOnlyMap[Int, EnableWinsFlag]].empty)

    val k1: Int = 1
    val e1      = EnableWinsFlag.empty
    val delta_1: Dotted[GrowOnlyMap[Int, EnableWinsFlag]] =
      emptyMap.mod(_.mutateKeyNamedCtx(k1, e1)(_.mod(_.enable(using r1)())))
    assertEquals(delta_1.context.internal.size, 1)
    assertEquals(delta_1.context.max(r1.uid), Some(Dot(r1.uid, 0)))
    assertEquals(delta_1.data.keySet, Set(1))
    assertEquals(delta_1.data.get(1).map(_.read), Some(true))

    // delta_1 and delta_2 are in parallel

    val k2: Int = 2
    val e2      = EnableWinsFlag.empty
    val delta_2: Dotted[GrowOnlyMap[Int, EnableWinsFlag]] =
      emptyMap.mod(_.mutateKeyNamedCtx(k2, e2)(_.mod(_.enable(using r2)())))
    assertEquals(delta_2.context.internal.size, 1)
    assertEquals(delta_2.context.max(r2.uid), Some(Dot(r2.uid, 0)))
    assertEquals(delta_2.data.keySet, Set(2))
    assertEquals(delta_2.data.get(2).map(_.read), Some(true))

    val merged: Dotted[GrowOnlyMap[Int, EnableWinsFlag]] =
      Lattice.merge(delta_1, delta_2)
    assertEquals(merged.context.internal.size, 2)
    assertEquals(merged.context.max(r1.uid), Some(Dot(r1.uid, 0)))
    assertEquals(merged.context.max(r2.uid), Some(Dot(r2.uid, 0)))
    assertEquals(merged.data.keySet, Set(1, 2))
    assertEquals(merged.data.get(1).map(_.read), Some(true))
    assertEquals(merged.data.get(2).map(_.read), Some(true))

    val delta_1_diff_delta_2: Option[Dotted[GrowOnlyMap[Int, EnableWinsFlag]]] =
      Lattice.diff(delta_1, delta_2)
    assertEquals(delta_1_diff_delta_2.isDefined, true, "delta_2 is not contained in delta_1")

    val delta_2_diff_delta_1: Option[Dotted[GrowOnlyMap[Int, EnableWinsFlag]]] =
      Lattice.diff(delta_2, delta_1)
    assertEquals(delta_2_diff_delta_1.isDefined, true, "delta_1 is not contained in delta_2")

    val merged_diff_delta_1: Option[Dotted[GrowOnlyMap[Int, EnableWinsFlag]]] =
      Lattice.diff(merged, delta_1)
    assertEquals(merged_diff_delta_1.isDefined, false, "delta_1 should be contained in merged")

    val merged_diff_delta_2: Option[Dotted[GrowOnlyMap[Int, EnableWinsFlag]]] =
      Lattice.diff(merged, delta_2)
    assertEquals(merged_diff_delta_2.isDefined, false, "delta_2 should be contained in merged")
  }

}
