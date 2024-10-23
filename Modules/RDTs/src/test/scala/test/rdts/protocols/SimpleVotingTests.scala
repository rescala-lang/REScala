package test.rdts.protocols

import rdts.base.Lattice.syntax.merge
import rdts.base.{Lattice, LocalUid, Uid}
import rdts.datatypes.experiments.protocols.simplified.{LeaderElection, MultiRoundVoting, Participants, SimpleVoting}

class SimpleVotingTests extends munit.FunSuite {

  val id1 = LocalUid.gen()
  val id2 = LocalUid.gen()
  val id3 = LocalUid.gen()
  val id4 = LocalUid.gen()
  given Participants(Set(id1, id2, id3, id4).map(_.uid))

  test("Voting for 4 participants") {
    var voting: LeaderElection = SimpleVoting.unchanged
    voting = voting `merge` voting.voteFor(id1.uid)(using id1)
    assertEquals(voting.result, None)
    voting = voting `merge` voting.voteFor(id1.uid)(using id2) `merge` voting.voteFor(id1.uid)(using id3)
    assertEquals(voting.result, Some(id1.uid))
    // voting again does not change anything:
    assertEquals(voting.voteFor(id1.uid)(using id1), SimpleVoting.unchanged)
  }

  test("Multiroundvoting for 4 participants") {
    var voting: MultiRoundVoting[Uid] = MultiRoundVoting.unchanged
    // everybody voting for id1
    voting = voting `merge` voting.voteFor(id1.uid)(using id1)
    assertEquals(voting.result, None)
    voting = voting `merge` voting.voteFor(id1.uid)(using id2) `merge` voting.voteFor(id1.uid)(using id3)
    assertEquals(voting.result, Some(id1.uid))
    // releasing
    voting = voting `merge` voting.release
    assertEquals(voting.result, None)
    // voting with upkeep
    voting = voting `merge` voting.voteFor(id1.uid)(using id1)
    assertEquals(voting.result, None)
    voting = voting `merge` voting.upkeep(using id2) `merge` voting.upkeep(using id3)
    assertEquals(voting.result, Some(id1.uid))
    // majority not possible
    voting = voting `merge` voting.release
    voting = voting `merge` voting.voteFor(id1.uid)(using id1) `merge` voting.voteFor(id2.uid)(using
      id2
    ) `merge` voting.voteFor(id3.uid)(using id3)
    assert(!voting.checkIfMajorityPossible)
    // check that upkeep cleans
    voting = voting `merge` voting.upkeep(using id1)
    assertEquals(voting.rounds.counter, Integer.toUnsignedLong(3))
    assertEquals(voting.rounds.value, SimpleVoting(Set.empty))
  }
}
