import org.scalatest.FreeSpec
import rescala.deltacrdts.CausalCRDT._
import rescala.deltacrdts.{AddWinsSet, DeltaCRDT}


class CausalCRDTTests extends FreeSpec {

  "An AddWinsSet should support" - {
    val initial = AddWinsSet[String](Map(), Set(), Map())
    val elem = "Mop"
    val elem2 = "Mip"

    "adding elements" in {
      val d1 = initial.add(elem)
      val d2 = initial.add(elem2)
      val bothAdded = DeltaCRDT.applyΔ(DeltaCRDT.applyΔ(initial, d1), d2)
      assert(!initial.contains(elem))
      assert(bothAdded.contains(elem) && bothAdded.contains(elem2))
    }

    "removing elements" in {
      val d1 = initial.add(elem)
      val added: AddWinsSet[String] = DeltaCRDT.applyΔ(initial, d1)

      val d2 = added.remove(elem)
      val removed: AddWinsSet[String] = DeltaCRDT.applyΔ(added, d2)
      assert(!removed.contains(elem))
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


