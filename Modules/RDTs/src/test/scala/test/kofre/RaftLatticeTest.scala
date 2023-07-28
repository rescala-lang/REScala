package test.kofre

import kofre.base.Lattice
import kofre.datatypes.experiments.RaftState
import kofre.datatypes.experiments.RaftState.Vote

class RaftLatticeTest extends munit.FunSuite {

  test("basic interaction") {

    val initial =
      RaftState[String](Set("a", "b", "perm"), Set(Vote(0, "a", "a"), Vote(0, "a", "b"), Vote(0, "a", "perm")))

    assertEquals(initial.leader, "a")
    assertEquals(initial.nextProposal, 0)
    assertEquals(initial.consensusSize, 2)
    val proposition = initial.proposeDelta("a", "new proposal")

    val proposaled = Lattice.merge(initial, proposition)

    val p2 = proposaled.proposeDelta("a", "another proposal")

    val proposaled2 = Lattice.merge(proposaled, p2)

    assertEquals(proposaled2.values, List())

    val s1 = proposaled2.supportProposalDelta("b")
    val s2 = proposaled2.supportProposalDelta("perm")

    assertEquals(Lattice.merge(proposaled2, s1).values, List("new proposal", "another proposal"))
    assertEquals(Lattice.merge(proposaled2, s2).values, List("new proposal", "another proposal"))
    assertEquals(Lattice.merge(Lattice.merge(proposaled2, s2), s1).values, List("new proposal", "another proposal"))

  }

  test("another interaction") {
    val participants = Set("a", "b", "perm").map(kofre.base.Uid.predefined)
    val afterFirstVote = RaftState[String](participants)
      .becomeCandidate("a")
      .supportLeader("b")
      .supportLeader("perm")

    assertEquals(afterFirstVote.leader, "a")
    assertEquals(afterFirstVote.currentTerm, 1)

    // kinda split between a and b, but perm is still fine with everyone

    val afterProposalAndSplit = afterFirstVote
      .propose("a", "As first proposal")
      .becomeCandidate("b")
      .supportLeader("b")
      .supportProposal("perm")

    assertEquals(afterProposalAndSplit.leader, "a")
    assertEquals(afterProposalAndSplit.currentTerm, 1)
    assertEquals(afterProposalAndSplit.values, List("As first proposal"))

    val bsRiseToPower = afterProposalAndSplit
      .propose("b", "Bs proposal before acceptance")
      .propose("a", "As second proposal while still leader")
      .supportLeader("perm")
      .propose("b", "Bs proposal after acceptance")
      .supportProposal("perm")

    assertEquals(bsRiseToPower.leader, "b")
    assertEquals(bsRiseToPower.currentTerm, 2)
    assertEquals(bsRiseToPower.values, List("As first proposal", "Bs proposal after acceptance"))

  }

}
