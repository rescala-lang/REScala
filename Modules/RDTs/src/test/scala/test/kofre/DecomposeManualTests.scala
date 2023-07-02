package test.kofre

import kofre.base.Bottom
import kofre.base.Lattice
import kofre.datatypes.GrowOnlyCounter
import kofre.datatypes.GrowOnlyList
import kofre.datatypes.GrowOnlyMap
import kofre.datatypes.GrowOnlySet
import kofre.datatypes.PosNegCounter
import kofre.datatypes.contextual.CausalQueue
import kofre.datatypes.contextual.LastWriterWins
import kofre.datatypes.contextual.MultiVersionRegister
import kofre.dotted.Dotted
import kofre.dotted.DottedLattice
import kofre.dotted.HasDots
import kofre.syntax.ReplicaId
import kofre.time.VectorClock
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

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

    val val_1: Dotted[LastWriterWins[Int]] = empty.write(using r1)(1)
    assertEquals(val_1.read, 1)

    Thread.sleep(1)

    val val_2: Dotted[LastWriterWins[Int]] = empty.write(using r2)(2)
    assertEquals(val_2.read, 2)

    val merged: Dotted[LastWriterWins[Int]] = Lattice[Dotted[LastWriterWins[Int]]].merge(val_1, val_2)
    assertEquals(merged.read, 2)

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

    val decomposed: Seq[GrowOnlySet[Int]] = Lattice[GrowOnlySet[Int]].decompose(merged).toSeq.sortBy(_.elements.headOption)
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).elements, Set(1))
    assertEquals(decomposed(1).elements, Set(2))
  }

  test("Dotted[GrowOnlyMap[Int, String]] decomposition") {
    import GrowOnlyMap.given

    given stringOrdering: Ordering[String] = scala.math.Ordering.String

    given stringLattice: Lattice[String] = Lattice.fromOrdering(stringOrdering)

    given stringDottedLattice: DottedLattice[String] = DottedLattice.liftLattice

    given HasDots[String] = HasDots.noDots

    given bottomString: Bottom[String] with {
      override def empty: String = ""
    }

    val empty: Dotted[GrowOnlyMap[Int, String]] = Dotted(Bottom[GrowOnlyMap[Int, String]].empty)

    val k1: Int = 1
    val v1: String = "one"
    val val_1: Dotted[GrowOnlyMap[Int, String]] = empty.mutateKeyNamedCtx(k1, v1)(e => e)
    assertEquals(val_1.data, Map(1 -> "one"))

    val k2: Int = 2
    val v2: String = "two"
    val val_2: Dotted[GrowOnlyMap[Int, String]] = empty.mutateKeyNamedCtx(k2, v2)(e => e)
    assertEquals(val_2.data, Map(2 -> "two"))

    val merged: Dotted[GrowOnlyMap[Int, String]] = Lattice[Dotted[GrowOnlyMap[Int, String]]].merge(val_1, val_2)
    assertEquals(merged.data, Map(1 -> "one", 2 -> "two"))

    val decomposed: Seq[Dotted[GrowOnlyMap[Int, String]]] = Lattice[Dotted[GrowOnlyMap[Int, String]]].decompose(merged).toSeq.sortBy(_.data.keys.headOption)
    assertEquals(decomposed.size, 2)
    assertEquals(decomposed(0).data, Map(1 -> "one"))
    assertEquals(decomposed(1).data, Map(2 -> "two"))
  }

}
