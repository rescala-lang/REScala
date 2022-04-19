package test.kofre

import org.scalacheck.Arbitrary
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import kofre.dotbased.{AddWinsSet, DotStore}
import kofre.Lattice.Operators
import kofre.causality.{CausalContext, Dot}

import scala.util.Random

object AWTestHelper {
  def merge[T](crdt: AddWinsSet[T], delta: AddWinsSet[T]): AddWinsSet[T] = {
    DotStore.DotMapInstance[T, CausalContext].merge(crdt, delta)
  }
}

class AddWinsSetTest extends AnyFreeSpec with ScalaCheckPropertyChecks {
  implicit lazy val addWinsSetWithStrings: Arbitrary[AddWinsSet[String]] = Arbitrary(for {
    values <- Arbitrary.arbitrary[Set[String]]
  } yield values.foldLeft(AddWinsSet.empty[String])((set, value) => AWTestHelper.merge(set, set.addRandom(value))))

  implicit lazy val addWinsSetWithInts: Arbitrary[AddWinsSet[Int]] = Arbitrary(for {
    values <- Arbitrary.arbitrary[Set[Int]]
  } yield values.foldLeft(AddWinsSet.empty[Int])((set, value) => AWTestHelper.merge(set, set.addRandom(value))))

  "An AddWinsSet should support" - {
    val initial = AddWinsSet[String](Map(), CausalContext.empty)
    val elem    = "Mop"
    val elem2   = "Mip"

    "adding elements" - {
      "manual tests" in {
        val d1        = initial.addRandom(elem)
        val d2        = initial.addRandom(elem2)
        val bothAdded = AWTestHelper.merge(AWTestHelper.merge(initial, d1), d2)
        assert(!initial.contains(elem))
        assert(bothAdded.contains(elem) && bothAdded.contains(elem2))
      }
      "property based tests" - {
        "with strings" in forAll { (s: AddWinsSet[String], elem: String) =>
          val delta = s.addRandom(elem)
          val added = AWTestHelper.merge(s, delta)
          assert(added.contains(elem))
        }
        "with integers" in forAll { (s: AddWinsSet[Int], elem: Int) =>
          val delta = s.addRandom(elem)
          val added = AWTestHelper.merge(s, delta)
          assert(added.contains(elem))
        }
      }

    }

    "removing elements" - {
      "manual tests" in {
        val d1                        = initial.addRandom(elem)
        val added: AddWinsSet[String] = AWTestHelper.merge(initial, d1)

        val d2                          = added.removeΔ(elem)
        val removed: AddWinsSet[String] = AWTestHelper.merge(added, d2)
        assert(!removed.contains(elem))
      }
      "property based tests" - {
        "with strings" in forAll { (s: AddWinsSet[String]) =>
          whenever(s.toSet.nonEmpty) {
            // randomly pick one element:
            val elem: String                = Random.shuffle(s.toSet.toList).head
            val delta                       = s.removeΔ(elem)
            val removed: AddWinsSet[String] = AWTestHelper.merge(s, delta)
            assert(!removed.contains(elem))
          }
        }
        "with integers" in forAll { (s: AddWinsSet[Int]) =>
          whenever(s.toSet.nonEmpty) {
            // randomly pick one element:
            val elem: Int                = Random.shuffle(s.toSet.toList).head
            val delta                    = s.removeΔ(elem)
            val removed: AddWinsSet[Int] = AWTestHelper.merge(s, delta)
            assert(!removed.contains(elem))
          }
        }
      }

      "clearing all elements" in {
        val d1        = initial.addRandom(elem)
        val d2        = initial.addRandom(elem2)
        val bothAdded = AWTestHelper.merge(AWTestHelper.merge(initial, d1), d2)

        val d3      = bothAdded.clear
        val cleared = AWTestHelper.merge(bothAdded, d3)
        assert(!cleared.contains(elem) && !cleared.contains(elem2))
      }

      "merging" - {
        "with itself" in {
          initial.merge(initial).toSet
        }

        "with other instances" in {
          val d1        = initial.addRandom(elem)
          val d2        = initial.addRandom(elem2)
          val bothAdded = AWTestHelper.merge(AWTestHelper.merge(initial, d1), d2)
          val merged    = initial.merge(bothAdded)
          assert(merged.contains(elem) && merged.contains(elem2))

          val d3      = bothAdded.clear
          val cleared = AWTestHelper.merge(bothAdded, d3)
          val merged2 = merged.merge(cleared).toSet
          assert(!merged2.contains(elem) && !merged2.contains(elem2))
        }
      }
    }
  }
}
