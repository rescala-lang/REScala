package test.rdts.protocols

import rdts.base.Lattice.merge
import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.GrowOnlyMap.*
import rdts.datatypes.experiments.protocols.paper.Paxos
import rdts.datatypes.experiments.protocols.Participants
import rdts.datatypes.{GrowOnlyCounter, GrowOnlyMap}
import rdts.dotted.{Dotted, DottedLattice}
import rdts.time.Dots

class PaperPaxosTest2 extends munit.FunSuite {
  given Bottom[Int] with
    override def empty: Int = Int.MinValue

  given dots: Dots = Dots.empty
  val id1          = LocalUid.gen()
  val id2          = LocalUid.gen()
  val id3          = LocalUid.gen()

  given participants: Participants = Participants(Set(id1, id2, id3).map(_.uid))

  var emptyPaxosObject: Paxos[Int] = Paxos()


  test("Paxos for 3 participants without errors") {
    var a: Paxos[Int] = emptyPaxosObject

    a = a `merge` a.propose(1)(using id1)
    a = a `merge` a.upkeep()(using id1) `merge` a.upkeep()(using id2) `merge` a.upkeep()(using id3)
    assertEquals(a.decision, None)
    a = a `merge` a.upkeep()(using id1)
    a = a `merge` a.upkeep()(using id1) `merge` a.upkeep()(using id2) `merge` a.upkeep()(using id3)
    assertEquals(a.decision, Some(1))
  }
  test("Upkeep on empty") {
    var a: Paxos[Int] = emptyPaxosObject
    assert(!a.currentRoundHasCandidate)
    a.upkeep()(using id1)
  }
}
