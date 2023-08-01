package test.kofre.bespoke

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.contextual.{CausalQueue, MultiVersionRegister}
import kofre.datatypes.{GrowOnlyCounter, GrowOnlyList, GrowOnlyMap, GrowOnlySet, LastWriterWins, PosNegCounter}
import kofre.dotted.{Dotted, DottedLattice, HasDots}
import kofre.syntax.ReplicaId
import kofre.time.{Dot, VectorClock}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.*

class DecomposeManualTests extends munit.ScalaCheckSuite {

  val r1: ReplicaId = ReplicaId.gen()
  val r2: ReplicaId = ReplicaId.gen()
  assert(r1 != r2)

  test("GrowOnlyCounter decomposition") {
    val empty: GrowOnlyCounter = Bottom[GrowOnlyCounter].empty

    val val_1: GrowOnlyCounter = empty.inc()(using r1)
    assertEquals(val_1.value, 1)

    val val_2: GrowOnlyCounter = empty.inc()(using r2)
    assertEquals(val_2.value, 1)

    val merged: GrowOnlyCounter = Lattice[GrowOnlyCounter].merge(val_1, val_2)
    assertEquals(merged.value, 2)

    val val_1_diff_val_2: Option[GrowOnlyCounter] = Lattice[GrowOnlyCounter].diff(val_1, val_2)
    assertEquals(val_1_diff_val_2, Some(val_2), "val_2 is not contained in val_1")

    val merged_diff_val_1: Option[GrowOnlyCounter] = Lattice[GrowOnlyCounter].diff(merged, val_1)
    assertEquals(merged_diff_val_1, None, "val_1 should be contained in merged")

    val merged_diff_val_2: Option[GrowOnlyCounter] = Lattice[GrowOnlyCounter].diff(merged, val_2)
    assertEquals(merged_diff_val_2, None, "val_2 should be contained in merged")

    val decomposed: Seq[GrowOnlyCounter] = Lattice[GrowOnlyCounter].decompose(merged).toSeq
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).value, 1)
    assertEquals(decomposed(1).value, 1)
  }

  test("PosNegCounter decomposition") {
    val empty: PosNegCounter = Bottom[PosNegCounter].empty

    val val_1: PosNegCounter = empty.inc()(using r1)
    assertEquals(val_1.value, 1)

    val val_2: PosNegCounter = empty.dec()(using r2)
    assertEquals(val_2.value, -1)

    val merged: PosNegCounter = Lattice[PosNegCounter].merge(val_1, val_2)
    assertEquals(merged.value, 0)

    val val_1_diff_val_2: Option[PosNegCounter] = Lattice[PosNegCounter].diff(val_1, val_2)
    assertEquals(val_1_diff_val_2, Some(val_2), "val_2 is not contained in val_1")

    val merged_diff_val_1: Option[PosNegCounter] = Lattice[PosNegCounter].diff(merged, val_1)
    assertEquals(merged_diff_val_1, None, "val_1 should be contained in merged")

    val merged_diff_val_2: Option[PosNegCounter] = Lattice[PosNegCounter].diff(merged, val_2)
    assertEquals(merged_diff_val_2, None, "val_2 should be contained in merged")

    val decomposed: Seq[PosNegCounter] = Lattice[PosNegCounter].decompose(merged).toSeq.sortBy(_.value)
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).value, -1)
    assertEquals(decomposed(1).value, 1)
  }

  test("Dotted[MultiVersionRegister[Int]] decomposition") {

    val empty: Dotted[MultiVersionRegister[Int]] = Dotted(Bottom[MultiVersionRegister[Int]].empty)

    val val_1: Dotted[MultiVersionRegister[Int]] = empty.write(using r1)(1)
    assertEquals(val_1.read, Set(1))

    val val_2: Dotted[MultiVersionRegister[Int]] = empty.write(using r2)(2)
    assertEquals(val_2.read, Set(2))

    val merged: Dotted[MultiVersionRegister[Int]] = Lattice[Dotted[MultiVersionRegister[Int]]].merge(val_1, val_2)
    assertEquals(merged.read, Set(1, 2))

    val val_1_diff_val_2: Option[Dotted[MultiVersionRegister[Int]]] = Lattice[Dotted[MultiVersionRegister[Int]]].diff(val_1, val_2)
    assertEquals(val_1_diff_val_2, Some(val_2), "val_2 is not contained in val_1")

    val merged_diff_val_1: Option[Dotted[MultiVersionRegister[Int]]] = Lattice[Dotted[MultiVersionRegister[Int]]].diff(merged, val_1)
    assertEquals(merged_diff_val_1, None, "val_1 should be contained in merged")

    val merged_diff_val_2: Option[Dotted[MultiVersionRegister[Int]]] = Lattice[Dotted[MultiVersionRegister[Int]]].diff(merged, val_2)
    assertEquals(merged_diff_val_2, None, "val_2 should be contained in merged")

    val decomposed: Seq[Dotted[MultiVersionRegister[Int]]] = Lattice[Dotted[MultiVersionRegister[Int]]].decompose(merged).toSeq.sortBy(_.data.read.headOption)
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).read, Set(1))
    assertEquals(decomposed(1).read, Set(2))
  }

  test("Dotted[LastWriterWins[Int]] decomposition") {
    given bottomInt: Bottom[Int] with {
      override def empty: Int = 0
    }

    val empty: Dotted[LastWriterWins[Int]] = Bottom[Dotted[LastWriterWins[Int]]].empty

    val val_1: Dotted[LastWriterWins[Int]] = empty.write(1)
    assertEquals(val_1.read, 1)

    val val_2: Dotted[LastWriterWins[Int]] = empty.write(2)
    assertEquals(val_2.read, 2)

    val merged: Dotted[LastWriterWins[Int]] = Lattice[Dotted[LastWriterWins[Int]]].merge(val_1, val_2)
    assertEquals(merged.read, 2)

    val val_1_diff_val_2: Option[Dotted[LastWriterWins[Int]]] = Lattice[Dotted[LastWriterWins[Int]]].diff(val_1, val_2)
    assertEquals(val_1_diff_val_2, Some(val_2), "val_2 is not contained in val_1")

    val merged_diff_val_1: Option[Dotted[LastWriterWins[Int]]] = Lattice[Dotted[LastWriterWins[Int]]].diff(merged, val_1)
    assertEquals(merged_diff_val_1, None, "val_1 should be contained in merged")

    val merged_diff_val_2: Option[Dotted[LastWriterWins[Int]]] = Lattice[Dotted[LastWriterWins[Int]]].diff(merged, val_2)
    assertEquals(merged_diff_val_2, None, "val_2 should be contained in merged")

    val decomposed: Seq[Dotted[LastWriterWins[Int]]] = Lattice[Dotted[LastWriterWins[Int]]].decompose(merged).toSeq
    assertEquals(decomposed.size, 1)
    assertEquals(decomposed.head.read, 2)
  }

  test("GrowOnlySet[Int] decomposition") {
    import GrowOnlySet.given

    val empty: GrowOnlySet[Int] = Bottom[GrowOnlySet[Int]].empty

    val val_1: GrowOnlySet[Int] = empty.insert(1)
    assertEquals(val_1.elements, Set(1))

    val val_2: GrowOnlySet[Int] = empty.insert(2)
    assertEquals(val_2.elements, Set(2))

    val merged: GrowOnlySet[Int] = Lattice[GrowOnlySet[Int]].merge(val_1, val_2)
    assertEquals(merged.elements, Set(1, 2))

    val val_1_diff_val_2: Option[GrowOnlySet[Int]] = Lattice[GrowOnlySet[Int]].diff(val_1, val_2)
    assertEquals(val_1_diff_val_2, Some(val_2), "val_2 is not contained in val_1")

    val merged_diff_val_1: Option[GrowOnlySet[Int]] = Lattice[GrowOnlySet[Int]].diff(merged, val_1)
    assertEquals(merged_diff_val_1, None, "val_1 should be contained in merged")

    val merged_diff_val_2: Option[GrowOnlySet[Int]] = Lattice[GrowOnlySet[Int]].diff(merged, val_2)
    assertEquals(merged_diff_val_2, None, "val_2 should be contained in merged")

    val decomposed: Seq[GrowOnlySet[Int]] = Lattice[GrowOnlySet[Int]].decompose(merged).toSeq.sortBy(_.elements.headOption)
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).elements, Set(1))
    assertEquals(decomposed(1).elements, Set(2))
  }

  test("Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] decomposition") {
    import GrowOnlyMap.given

    given stringOrdering: Ordering[String] = scala.math.Ordering.String

    given stringLattice: Lattice[String] = Lattice.fromOrdering(stringOrdering)

    given HasDots[String] = HasDots.noDots

    given bottomString: Bottom[String] with {
      override def empty: String = ""
    }

    val emptyMap: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] = Dotted(Bottom[GrowOnlyMap[Int, LastWriterWins[String]]].empty)

    val k1: Int = 1
    val v1: String = "one"
    val e1 = LastWriterWins.now("")
    val val_1: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] = emptyMap.mutateKeyNamedCtx(k1, e1)(_.write(v1))
    assertEquals(val_1.data.keySet, Set(1))
    assertEquals(val_1.data.get(1).map(_.payload), Some("one"))

    val k2: Int = 2
    val v2: String = "two"
    val e2 = LastWriterWins.now("")
    val val_2: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] = emptyMap.mutateKeyNamedCtx(k2, e2)(_.write(v2))
    assertEquals(val_2.data.keySet, Set(2))
    assertEquals(val_2.data.get(2).map(_.payload), Some("two"))

    val merged: Dotted[GrowOnlyMap[Int, LastWriterWins[String]]] = Lattice[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]].merge(val_1, val_2)
    assertEquals(merged.data.keySet, Set(1, 2))
    assertEquals(merged.data.get(1).map(_.payload), Some("one"))
    assertEquals(merged.data.get(2).map(_.payload), Some("two"))

    val val_1_diff_val_2: Option[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]] = Lattice[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]].diff(val_1, val_2)
    assertEquals(val_1_diff_val_2, Some(val_2), "val_2 is not contained in val_1")

    val merged_diff_val_1: Option[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]] = Lattice[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]].diff(merged, val_1)
    assertEquals(merged_diff_val_1, None, "val_1 should be contained in merged")

    val merged_diff_val_2: Option[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]] = Lattice[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]].diff(merged, val_2)
    assertEquals(merged_diff_val_2, None, "val_2 should be contained in merged")

    val decomposed: Seq[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]] = Lattice[Dotted[GrowOnlyMap[Int, LastWriterWins[String]]]].decompose(merged).toSeq.sortBy(_.data.keys.headOption)
    assertEquals(decomposed.size, 2)

    assertEquals(decomposed(0).data.get(1).map(_.payload), Some("one"))
    assertEquals(decomposed(0).context.internal.keySet.size, 0)

    assertEquals(decomposed(1).data.get(2).map(_.payload), Some("two"))
    assertEquals(decomposed(1).context.internal.keySet.size, 0)
  }

}
