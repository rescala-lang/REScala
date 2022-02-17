package test.kofre

import org.scalacheck.Arbitrary
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import kofre.IdUtil
import kofre.Lattice.merge
import kofre.dotbased.{AddWinsSetO}
import kofre.causality.CausalContext
import kofre.Lattice.Operators

import scala.util.Random

object AWOTestHelper {
  def merge[T](crdt: AddWinsSetO[T], delta: AddWinsSetO[T]): AddWinsSetO[T] = {
    AddWinsSetO.latticeAddWinsSetPerfOpt.merge(crdt, delta)
  }
}

class AddWinsSetOTest extends AnyFreeSpec with ScalaCheckPropertyChecks {
  implicit lazy val AddWinsSetOWithStrings: Arbitrary[AddWinsSetO[String]] = Arbitrary(for {
    values <- Arbitrary.arbitrary[Set[String]]
  } yield values.foldLeft(AddWinsSetO.empty[String])((set, value) =>
    AWOTestHelper.merge(set, set.add(value, IdUtil.genId()))
  ))

  implicit lazy val AddWinsSetOWithInts: Arbitrary[AddWinsSetO[Int]] = Arbitrary(for {
    values <- Arbitrary.arbitrary[Set[Int]]
  } yield values.foldLeft(AddWinsSetO.empty[Int])((set, value) =>
    AWOTestHelper.merge(set, set.add(value, IdUtil.genId()))
  ))

  "An AddWinsSetO should support" - {
    val initial = AddWinsSetO[String](Map(), CausalContext(Map.empty))
    val elem    = "Mop"
    val elem2   = "Mip"

    "adding elements" - {
      "manual tests" ignore {
        val id1       = IdUtil.genId()
        val id2       = IdUtil.genId()
        val d1        = initial.add(elem, id1)
        val d2        = initial.add(elem2, id2)
        val bothAdded = AWOTestHelper.merge(AWOTestHelper.merge(initial, d1), d2)
        assert(!initial.contains(elem))
        assert(bothAdded.contains(elem) && bothAdded.contains(elem2))
      }
      "property based tests" - {
        "with strings" ignore forAll { (s: AddWinsSetO[String], elem: String) =>
          val delta = s.add(elem, IdUtil.genId())
          val added = AWOTestHelper.merge(s, delta)
          assert(added.contains(elem))
        }
        "with integers" ignore forAll { (s: AddWinsSetO[Int], elem: Int) =>
          val delta = s.add(elem, IdUtil.genId())
          val added = AWOTestHelper.merge(s, delta)
          assert(added.contains(elem))
        }
      }

    }

    "removing elements" - {
      "manual tests" ignore {
        val d1                         = initial.add(elem, IdUtil.genId())
        val added: AddWinsSetO[String] = AWOTestHelper.merge(initial, d1)

        val d2                           = added.removeΔ(elem)
        val removed: AddWinsSetO[String] = AWOTestHelper.merge(added, d2)
        assert(!removed.contains(elem))
      }
      "property based tests" - {
        "with strings" ignore forAll { (s: AddWinsSetO[String]) =>
          whenever(s.toSet.nonEmpty) {
            // randomly pick one element:
            val elem: String                 = Random.shuffle(s.toSet.toList).head
            val delta                        = s.removeΔ(elem)
            val removed: AddWinsSetO[String] = AWOTestHelper.merge(s, delta)
            assert(!removed.contains(elem))
          }
        }
        "with integers" ignore forAll { (s: AddWinsSetO[Int]) =>
          whenever(s.toSet.nonEmpty) {
            // randomly pick one element:
            val elem: Int                 = Random.shuffle(s.toSet.toList).head
            val delta                     = s.removeΔ(elem)
            val removed: AddWinsSetO[Int] = AWOTestHelper.merge(s, delta)
            assert(!removed.contains(elem))
          }
        }
      }

      "clearing all elements" ignore {
        val d1        = initial.add(elem, IdUtil.genId())
        val d2        = initial.add(elem2, IdUtil.genId())
        val bothAdded = AWOTestHelper.merge(AWOTestHelper.merge(initial, d1), d2)

        val d3      = bothAdded.clear
        val cleared = AWOTestHelper.merge(bothAdded, d3)
        assert(!cleared.contains(elem) && !cleared.contains(elem2))
      }

      "merging" - {
        "with itself" ignore {
          initial.merge(initial).toSet
        }

        "with other instances" ignore {
          val d1        = initial.add(elem, IdUtil.genId())
          val d2        = initial.add(elem2, IdUtil.genId())
          val bothAdded = AWOTestHelper.merge(AWOTestHelper.merge(initial, d1), d2)
          val merged    = initial.merge(bothAdded)
          assert(merged.contains(elem) && merged.contains(elem2))

          val d3      = bothAdded.clear
          val cleared = AWOTestHelper.merge(bothAdded, d3)
          val merged2 = merged.merge(cleared).toSet
          assert(!merged2.contains(elem) && !merged2.contains(elem2))
        }
      }
    }
  }
}
