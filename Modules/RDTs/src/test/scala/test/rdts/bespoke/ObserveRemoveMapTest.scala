package test.rdts.bespoke

import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.contextual.ObserveRemoveMap
import rdts.dotted.Obrem
import rdts.time.Dot

class ObserveRemoveMapTest extends munit.FunSuite {

  given Lattice[Dot] = Lattice.assertEquals

  test("basic usage") {
    val obremmap = Obrem(ObserveRemoveMap.empty[String, Dot])

    given replicaId: LocalUid = LocalUid.gen()

    val added = obremmap.mod { ctx ?=> current =>
      val nextDot = ctx.nextDot(replicaId.uid)
      current.update("Hi!", nextDot)
    }

    assert(added.data.contains("Hi!"))

    val remove = added.mod { ctx ?=> current =>
      current.remove("Hi!")
    }

    val merged = added `merge` remove

    assertEquals(merged.data.entries.toMap, Map.empty)
  }
}
