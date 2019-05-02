import org.scalacheck.Arbitrary
import org.scalatest.FreeSpec
import org.scalatest.prop.PropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import rescala.deltacrdts.CausalCRDT._
import rescala.deltacrdts.{AddWinsSet, DeltaCRDT}

import scala.util.Random


class CausalCRDTTests extends FreeSpec with ScalaCheckPropertyChecks {
  implicit lazy val addWinsSetWithStrings: Arbitrary[AddWinsSet[String]] = Arbitrary(for {
    values <- Arbitrary.arbitrary[Set[String]]
  } yield values.foldLeft(AddWinsSet[String](Map(), Set(), Map()))
  ((set, value) => DeltaCRDT.applyΔ(set, set.add(value))))

  implicit lazy val addWinsSetWithInts: Arbitrary[AddWinsSet[Int]] = Arbitrary(for {
    values <- Arbitrary.arbitrary[Set[Int]]
  } yield values.foldLeft(AddWinsSet[Int](Map(), Set(), Map()))
  ((set, value) => DeltaCRDT.applyΔ(set, set.add(value))))

  "An AddWinsSet should support" - {
    val initial = AddWinsSet[String](Map(), Set(), Map())
    val elem = "Mop"
    val elem2 = "Mip"

    "adding elements" - {
      "manual tests" in {
        val d1 = initial.add(elem)
        val d2 = initial.add(elem2)
        val bothAdded = DeltaCRDT.applyΔ(DeltaCRDT.applyΔ(initial, d1), d2)
        assert(!initial.contains(elem))
        assert(bothAdded.contains(elem) && bothAdded.contains(elem2))
      }
      "property based tests" - {
        "with strings" in forAll { (s: AddWinsSet[String], elem: String) =>
          val delta = s.add(elem)
          val added = DeltaCRDT.applyΔ(s, delta)
          assert(added.contains(elem))
        }
        "with integers" in forAll { (s: AddWinsSet[Int], elem: Int) =>
          val delta = s.add(elem)
          val added = DeltaCRDT.applyΔ(s, delta)
          assert(added.contains(elem))
        }
      }

    }

    "removing elements" - {
      "manual tests" in {
        val d1 = initial.add(elem)
        val added: AddWinsSet[String] = DeltaCRDT.applyΔ(initial, d1)

        val d2 = added.remove(elem)
        val removed: AddWinsSet[String] = DeltaCRDT.applyΔ(added, d2)
        assert(!removed.contains(elem))
      }
      "property based tests" - {
        "with strings" in forAll { s: AddWinsSet[String] =>
          whenever(s.toSet.nonEmpty) {
            // randomly pick one element:
            val elem: String = Random.shuffle(s.toSet.toList).head
            val delta = s.remove(elem)
            val removed: AddWinsSet[String] = DeltaCRDT.applyΔ(s, delta)
            assert(!removed.contains(elem))
          }
        }
        "with integers" in forAll { s: AddWinsSet[Int] =>
          whenever(s.toSet.nonEmpty) {
            // randomly pick one element:
            val elem: Int = Random.shuffle(s.toSet.toList).head
            val delta = s.remove(elem)
            val removed: AddWinsSet[Int] = DeltaCRDT.applyΔ(s, delta)
            assert(!removed.contains(elem))
          }
        }
    }

    "clearing all elements" in {
      val d1 = initial.add(elem)
      val d2 = initial.add(elem2)
      val bothAdded = DeltaCRDT.applyΔ(DeltaCRDT.applyΔ(initial, d1), d2)

      val d3 = bothAdded.clear
      val cleared = DeltaCRDT.applyΔ(bothAdded, d3)
      assert(!cleared.contains(elem) && !cleared.contains(elem2))
    }

    "merging" - {
      "with itself" in {
        initial.merge(initial).toSet
      }

      "with other instances" in {
        val d1 = initial.add(elem)
        val d2 = initial.add(elem2)
        val bothAdded = DeltaCRDT.applyΔ(DeltaCRDT.applyΔ(initial, d1), d2)
        val merged = initial.merge(bothAdded)
        assert(merged.contains(elem) && merged.contains(elem2))

        val d3 = bothAdded.clear
        val cleared = DeltaCRDT.applyΔ(bothAdded, d3)
        val merged2 = merged.merge(cleared).toSet
        assert(!merged2.contains(elem) && !merged2.contains(elem2))
      }
    }
  }
  }
}


