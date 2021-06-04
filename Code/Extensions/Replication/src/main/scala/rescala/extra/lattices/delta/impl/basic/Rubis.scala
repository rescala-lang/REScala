package rescala.extra.lattices.delta.impl.basic

import rescala.extra.lattices.delta.{CContext, UIJDLattice}
import rescala.extra.lattices.delta.crdt.Bid.User
import rescala.extra.lattices.delta.crdt.RubisCRDT
import rescala.extra.lattices.delta.crdt.RubisCRDT.AID

class Rubis[C: CContext](crdt: DeltaCRDT[Rubis.State[C]]) {
  def placeBid(auctionId: AID, userId: User, price: Int): Rubis[C] =
    new Rubis(crdt.mutate(RubisCRDT.placeBid(auctionId, userId, price)))

  def closeAuction(auctionId: AID): Rubis[C] = new Rubis(crdt.mutate(RubisCRDT.closeAuction(auctionId)))

  def openAuction(auctionId: AID): Rubis[C] = new Rubis(crdt.mutate(RubisCRDT.openAuction(auctionId)))

  def requestRegisterUser(userId: User): Rubis[C] = new Rubis(crdt.mutate(RubisCRDT.requestRegisterUser(userId)))

  def resolveRegisterUser(): Rubis[C] = new Rubis(crdt.mutate(RubisCRDT.resolveRegisterUser()))

  def printState(): Unit = println(crdt.state)

  def processReceivedDeltas(): Rubis[C] = new Rubis(crdt.processReceivedDeltas())
}

object Rubis {
  type State[C] = RubisCRDT.State[C]

  implicit val UserAsUIJDLattice: UIJDLattice[User] = UIJDLattice.AtomicUIJDLattice[User]

  def apply[C: CContext](ae: AntiEntropy[State[C]]): Rubis[C] =
    new Rubis(DeltaCRDT.empty(ae))
}
