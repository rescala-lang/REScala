package replication.protocols
import rdts.base.Bottom
import rdts.base.Lattice.merge
import rdts.datatypes.GrowOnlyMap.*
import rdts.datatypes.{GrowOnlyCounter, GrowOnlyMap}
import rdts.dotted.{Dotted, DottedLattice}
import rdts.syntax.LocalUid
import rdts.time.Dots
import replication.protocols.Paxos.*

class PaxosTest extends munit.FunSuite {
  given Bottom[Int] with
    override def empty: Int = Int.MinValue

  given dots: Dots = Dots.empty
  val id1          = LocalUid.gen()
  val id2          = LocalUid.gen()
  val id3          = LocalUid.gen()

  test("Paxos for 3 participants without errors") {
    var a: Paxos[Int, 2] = Paxos.unchanged(using 2)

    a = a merge a.prepare()(using id1)
    a = a merge a.upkeep()(using id1) merge a.upkeep()(using id2) merge a.upkeep()(using id3)
    assertEquals(a.read, None)
    a = a merge a.accept(1)(using id1)
    a = a merge a.upkeep()(using id1) merge a.upkeep()(using id2) merge a.upkeep()(using id3)
    assertEquals(a.read, Some(1))
  }

}
