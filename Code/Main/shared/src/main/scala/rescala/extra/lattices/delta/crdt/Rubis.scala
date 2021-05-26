package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rescala.extra.lattices.delta.DeltaCRDT.DeltaMutator
import rescala.extra.lattices.delta.{AntiEntropy, DeltaCRDT, UIJDLattice}
import rescala.extra.lattices.delta.crdt.Bid.User
import rescala.extra.lattices.delta.crdt.RubisCRDT.AID
import rescala.extra.lattices.delta.UIJDLattice._

object RubisCRDT {
  type AID = String

  type State = (GSet.State[User], Map[AID, Auction.State])

  def placeBid(auctionId: AID, userId: User, price: Int): DeltaMutator[State] = {
    case (replicaID, (users, m)) =>
      val newMap =
        if (users.contains(userId) && m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => AuctionCRDT.bid(userId, price)(replicaID, a)) }
        } else Map.empty[AID, Auction.State]

      (UIJDLattice[GSet.State[User]].bottom, newMap)
  }

  def closeAuction(auctionId: AID): DeltaMutator[State] = {
    case (replicaID, (_, m)) =>
      val newMap =
        if (m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => AuctionCRDT.close()(replicaID, a)) }
        } else Map.empty[AID, Auction.State]

      (UIJDLattice[GSet.State[User]].bottom, newMap)
  }

  def openAuction(auctionId: AID): DeltaMutator[State] = {
    case (_, (_, m)) =>
      val newMap =
        if (m.contains(auctionId)) Map.empty[AID, Auction.State]
        else Map(auctionId -> UIJDLattice[Auction.State].bottom)

      (UIJDLattice[GSet.State[User]].bottom, newMap)
  }

  def registerUser(userId: User): DeltaMutator[State] =
    (replicaID, _) => (GSetCRDT.insert(userId)(replicaID, UIJDLattice[GSet.State[User]].bottom), Map())
}

class Rubis(crdt: DeltaCRDT[Rubis.State]) {
  def placeBid(auctionId: AID, userId: User, price: Int): Rubis =
    new Rubis(crdt.mutate(RubisCRDT.placeBid(auctionId, userId, price)))

  def closeAuction(auctionId: AID): Rubis = new Rubis(crdt.mutate(RubisCRDT.closeAuction(auctionId)))

  def openAuction(auctionId: AID): Rubis = new Rubis(crdt.mutate(RubisCRDT.openAuction(auctionId)))

  def registerUser(userId: User): Rubis = new Rubis(crdt.mutate(RubisCRDT.registerUser(userId)))

  def printState(): Unit = println(crdt.state)

  def processReceivedDeltas(): Rubis = new Rubis(crdt.processReceivedDeltas())
}

object Rubis {
  type State = RubisCRDT.State

  def apply(ae: AntiEntropy[State]): Rubis =
    new Rubis(DeltaCRDT.empty[State](ae))

  implicit val RubisStateCodec: JsonValueCodec[State] = JsonCodecMaker.make
}
