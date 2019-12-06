package tests.distribution

import org.scalatest.freespec.AnyFreeSpec
import rescala.extra.lattices.{Lattice, RaftState}
import rescala.extra.lattices.RaftState.Vote

class RaftLatticeTest  extends AnyFreeSpec {


  "basic interaction" in {

    val initial = RaftState[String](Set("a", "b", "c"),
                                    Set(Vote(0, "a", "a"), Vote(0, "a", "b"), Vote(0, "a", "c")))

    assert(initial.leader === "a")
    assert(initial.nextProposal === 0)
    assert(initial.consensusSize === 2)
    val proposition = initial.proposeDelta("a", "new proposal").get

    val proposaled = Lattice.merge(initial, proposition)

    val p2 = proposaled.proposeDelta("a", "another proposal").get

    val proposaled2 = Lattice.merge(proposaled, p2)

    assert(proposaled2.values === List())

    val s1 = proposaled2.supportVoteDelta("b")
    val s2 = proposaled2.supportVoteDelta("c")

    assert(Lattice.merge(proposaled2, s1).values === List("new proposal", "another proposal"))
    assert(Lattice.merge(proposaled2, s2).values  === List("new proposal", "another proposal"))
    assert(Lattice.merge(Lattice.merge(proposaled2, s2), s1).values  === List("new proposal", "another proposal"))

  }

}
