package replication.protocols
import rdts.base.{Lattice, Uid}
import rdts.datatypes.contextual.ReplicatedSet
import rdts.datatypes.{Epoch, LastWriterWins}
import rdts.dotted.{Dotted, DottedLattice}
import rdts.syntax.LocalUid
import rdts.time.Dots

class VotingTests2Participants extends munit.FunSuite {
  given dots: Dots              = Dots.empty
  given Lattice[Dotted[Voting]] = Lattice.derived
  // create replicas for set of 2 participants
  val id1: LocalUid = LocalUid.gen()
  val id2: LocalUid = LocalUid.gen()
  var voting = Dotted(Voting(
    rounds = Epoch.empty[ReplicatedSet[Vote]],
    numParticipants = LastWriterWins.now(2)
  ))
  test("No initial owner") {
    assert(!voting.data.isOwner(using id1))
  }
  test("Still not owner after one vote") {
    voting = voting.merge(voting.data.voteFor(id1.uid)(using id1))
    assert(!voting.data.isOwner(using id1))
  }
  test("Duplicate vote changes nothing") {
    assertEquals(voting.merge(voting.data.voteFor(id1.uid)(using id1)), voting)
  }
  test("Is owner after two votes") {
    voting = voting.merge(voting.data.voteFor(id1.uid)(using id2))
    assert(voting.data.isOwner(using id1))
    assert(!voting.data.isOwner(using id2))
  }
  test("Is not owner for 4 participants") {
    voting = voting.merge(Dotted(Voting(voting.data.rounds, LastWriterWins.now(4))))
    assert(!voting.data.isOwner(using id1))
    assert(!voting.data.isOwner(using id2))
  }
  test("Is owner for 3 participants") {
    voting = voting.merge(Dotted(Voting(voting.data.rounds, LastWriterWins.now(3))))
    assert(voting.data.isOwner(using id1))
    assert(!voting.data.isOwner(using id2))
  }
}
