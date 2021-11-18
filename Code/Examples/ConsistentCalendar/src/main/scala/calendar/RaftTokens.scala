package calendar

import rescala.extra.lattices.RaftState
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.crdt.reactive.AWSet

import scala.util.Random

case class Token(id: Long, owner: String, value: String) {
  def same(other: Token) = owner == other.owner && value == other.value
}

case class RaftTokens(replicaID: String,
                      tokenAgreement: RaftState[Token],
                      want: AWSet[Token, DietMapCContext],
                      tokenFreed: AWSet[Token, DietMapCContext]) {


  def owned(value: String): List[Token] = {
    tokenAgreement.values.filter(t => t.value == value && t.owner == replicaID && !tokenFreed.elements.contains(t))
  }

  def isOwned(value: String): Boolean = owned(value).nonEmpty

  def acquire(value: String): RaftTokens = {

    val token = Token(Random.nextLong(), replicaID, value)
    // conditional is only an optimization
    if (!(tokenAgreement.values.iterator ++ want.elements.iterator).exists(_.same(token))) {
      copy(want = want.add(token))
    }
    else this
  }

  def free(value: String): RaftTokens = {
    copy(tokenFreed = tokenFreed.addAll(owned(value)))
  }

  def update(): RaftTokens = {

    val generalDuties = tokenAgreement.supportLeader(replicaID).supportProposal(replicaID)

    if (tokenAgreement.leader == replicaID) {
      val unwanted = want.removeAll(want.elements.filter(generalDuties.values.contains))
      want.elements.headOption match {
        case None => copy(tokenAgreement = generalDuties, want = unwanted)
        case Some(tok) =>
          copy(tokenAgreement = generalDuties.propose(replicaID, tok), want = unwanted)
      }
    } else copy(tokenAgreement = generalDuties)


  }


}

object RaftTokens {
  def init(replicaID: String) = RaftTokens(replicaID, RaftState(Set(replicaID)), AWSet(replicaID), AWSet(replicaID))
}
