package tests.distribution

import org.scalatest.freespec.AnyFreeSpec
import rescala.extra.lattices.{Lattice, RaftState}
import rescala.extra.lattices.RaftState.Vote

class RaftLatticeTest extends AnyFreeSpec {


  "basic interaction" in {

    val initial = RaftState[String](Set("a", "b", "c"),
                                    Set(Vote(0, "a", "a"), Vote(0, "a", "b"), Vote(0, "a", "c")))

    assert(initial.leader === "a")
    assert(initial.nextProposal === 0)
    assert(initial.consensusSize === 2)
    val proposition = initial.proposeDelta("a", "new proposal")

    val proposaled = Lattice.merge(initial, proposition)

    val p2 = proposaled.proposeDelta("a", "another proposal")

    val proposaled2 = Lattice.merge(proposaled, p2)

    assert(proposaled2.values === List())

    val s1 = proposaled2.supportProposalDelta("b")
    val s2 = proposaled2.supportProposalDelta("c")

    assert(Lattice.merge(proposaled2, s1).values === List("new proposal", "another proposal"))
    assert(Lattice.merge(proposaled2, s2).values === List("new proposal", "another proposal"))
    assert(Lattice.merge(Lattice.merge(proposaled2, s2), s1).values === List("new proposal",
                                                                             "another proposal"))

  }

  "another interaction" in {
    val participants   = Set("a", "b", "c")
    val afterFirstVote = RaftState[String](participants)
      .becomeCandidate("a")
      .supportLeader("b")
      .supportLeader("c")

    assert(afterFirstVote.leader === "a")
    assert(afterFirstVote.currentTerm === 1)

    //kinda split between a and b, but c is still fine with everyone

    val afterProposalAndSplit = afterFirstVote
      .propose("a", "As first proposal")
      .becomeCandidate("b")
      .supportLeader("b")
      .supportProposal("c")

    assert(afterProposalAndSplit.leader == "a")
    assert(afterProposalAndSplit.currentTerm == 1)
    assert(afterProposalAndSplit.values == List("As first proposal"))

    val bsRiseToPower = afterProposalAndSplit
      .propose("b", "Bs proposal before acceptance")
      .propose("a", "As second proposal while still leader")
      .supportLeader("c")
      .propose("b", "Bs proposal after acceptance")
      .supportProposal("c")


    assert(bsRiseToPower.leader == "b")
    assert(bsRiseToPower.currentTerm == 2)
    assert(bsRiseToPower.values == List("As first proposal", "Bs proposal after acceptance"))


  }

}
