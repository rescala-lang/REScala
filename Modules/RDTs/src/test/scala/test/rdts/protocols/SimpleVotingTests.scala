package test.rdts.protocols

import rdts.base.{Lattice, LocalUid}
import rdts.base.Lattice.syntax.merge
import rdts.datatypes.experiments.protocols.{Voting}
import rdts.datatypes.experiments.protocols.simplified.{SimpleVoting, MultiRoundVoting}

class SimpleVotingTests extends munit.FunSuite {

  val id1 = LocalUid.gen()
  val id2 = LocalUid.gen()
  val id3 = LocalUid.gen()
  val id4 = LocalUid.gen()

  test("Voting for 4 participants") {
    var voting: SimpleVoting = SimpleVoting.unchanged
    voting = voting `merge` voting.voteFor(id1.uid)(using id1)
    assert(!voting.isLeader(using id1))
    voting = voting `merge` voting.voteFor(id1.uid)(using id2) `merge` voting.voteFor(id1.uid)(using id3)
    assert(voting.isLeader(using id1))
    assert(!voting.isLeader(using id2))

    // voting again does not change anything:
    assertEquals(voting.voteFor(id1.uid)(using id1), SimpleVoting.unchanged)
  }

  test("Multiroundvoting for 4 participants") {
    var voting: MultiRoundVoting = MultiRoundVoting.unchanged
    // everybody voting for id1
    voting = voting `merge` voting.voteFor(id1.uid)(using id1)
    assert(!voting.isLeader(using id1))
    voting = voting `merge` voting.voteFor(id1.uid)(using id2) `merge` voting.voteFor(id1.uid)(using id3)
    assert(voting.isLeader(using id1))
    assert(!voting.isLeader(using id2))
    // releasing
    voting = voting `merge` voting.release
    assert(!voting.isLeader(using id1))
    // voting with upkeep
    voting = voting `merge` voting.voteFor(id1.uid)(using id1)
    assert(!voting.isLeader(using id1))
    voting = voting `merge` voting.upkeep(using id2) `merge` voting.upkeep(using id3)
    assert(voting.isLeader(using id1))
    // majority not possible
    voting = voting `merge` voting.release
    voting = voting `merge` voting.voteFor(id1.uid)(using id1) `merge` voting.voteFor(id2.uid)(using
      id2
    ) `merge` voting.voteFor(id3.uid)(using id3)
    assert(!voting.checkIfMajorityPossible(using id1))
    // check that upkeep cleans
    voting = voting `merge` voting.upkeep(using id1)
    assertEquals(voting.rounds.counter, Integer.toUnsignedLong(3))
    assertEquals(voting.rounds.value, SimpleVoting(Set.empty))
  }
}
