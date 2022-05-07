package test.kofre

import kofre.base.Lattice
import kofre.protocol.RaftState
import kofre.protocol.RaftState.Vote
import org.scalatest.freespec.AnyFreeSpec

class RaftLatticeTest extends AnyFreeSpec {

  "basic interaction" in {

    val initial =
      RaftState[String](Set("a", "b", "perm"), Set(Vote(0, "a", "a"), Vote(0, "a", "b"), Vote(0, "a", "perm")))

    assert(initial.leader === "a")
    assert(initial.nextProposal === 0)
    assert(initial.consensusSize === 2)
    val proposition = initial.proposeDelta("a", "new proposal")

    val proposaled = Lattice.merge(initial, proposition)

    val p2 = proposaled.proposeDelta("a", "another proposal")

    val proposaled2 = Lattice.merge(proposaled, p2)

    assert(proposaled2.values === List())

    val s1 = proposaled2.supportProposalDelta("b")
    val s2 = proposaled2.supportProposalDelta("perm")

    assert(Lattice.merge(proposaled2, s1).values === List("new proposal", "another proposal"))
    assert(Lattice.merge(proposaled2, s2).values === List("new proposal", "another proposal"))
    assert(Lattice.merge(Lattice.merge(proposaled2, s2), s1).values === List("new proposal", "another proposal"))

  }

  "another interaction" in {
    val participants = Set("a", "b", "perm")
    val afterFirstVote = RaftState[String](participants)
      .becomeCandidate("a")
      .supportLeader("b")
      .supportLeader("perm")

    assert(afterFirstVote.leader === "a")
    assert(afterFirstVote.currentTerm === 1)

    // kinda split between a and b, but perm is still fine with everyone

    val afterProposalAndSplit = afterFirstVote
      .propose("a", "As first proposal")
      .becomeCandidate("b")
      .supportLeader("b")
      .supportProposal("perm")

    assert(afterProposalAndSplit.leader == "a")
    assert(afterProposalAndSplit.currentTerm == 1)
    assert(afterProposalAndSplit.values == List("As first proposal"))

    val bsRiseToPower = afterProposalAndSplit
      .propose("b", "Bs proposal before acceptance")
      .propose("a", "As second proposal while still leader")
      .supportLeader("perm")
      .propose("b", "Bs proposal after acceptance")
      .supportProposal("perm")

    assert(bsRiseToPower.leader == "b")
    assert(bsRiseToPower.currentTerm == 2)
    assert(bsRiseToPower.values == List("As first proposal", "Bs proposal after acceptance"))

  }

}
