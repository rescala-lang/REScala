package rescala.extra.lattices.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.DeltaCRDT.DeltaMutator
import rescala.extra.lattices.delta.{AntiEntropy, CContext, Causal, DeltaCRDT, Dot, UIJDLattice}
import rescala.extra.lattices.delta.crdt.Bid.User
import rescala.extra.lattices.delta.crdt.RubisCRDT.AID

object RubisCRDT {
  type AID = String

  type State[C] = (AWSet.State[(User, String), C], Map[User, String], Map[AID, Auction.State])

  implicit val UserAsUIJDLattice: UIJDLattice[User] = UIJDLattice.AtomicUIJDLattice[User]

  private def deltaState[C: CContext](
      userRequests: Option[AWSet.State[(User, String), C]] = None,
      users: Option[Map[User, String]] = None,
      auctions: Option[Map[AID, Auction.State]] = None
  ): State[C] = {
    val bottom = UIJDLattice[State[C]].bottom

    (
      userRequests.getOrElse(bottom._1),
      users.getOrElse(bottom._2),
      auctions.getOrElse(bottom._3)
    )
  }

  def placeBid[C: CContext](auctionId: AID, userId: User, price: Int): DeltaMutator[State[C]] = {
    case (replicaID, (_, users, m)) =>
      val newMap =
        if (users.get(userId).contains(replicaID) && m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => AuctionCRDT.bid(userId, price)(replicaID, a)) }
        } else Map.empty[AID, Auction.State]

      deltaState(auctions = Some(newMap))
  }

  def closeAuction[C: CContext](auctionId: AID): DeltaMutator[State[C]] = {
    case (replicaID, (_, _, m)) =>
      val newMap =
        if (m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => AuctionCRDT.close()(replicaID, a)) }
        } else Map.empty[AID, Auction.State]

      deltaState(auctions = Some(newMap))
  }

  def openAuction[C: CContext](auctionId: AID): DeltaMutator[State[C]] = {
    case (_, (_, _, m)) =>
      val newMap =
        if (m.contains(auctionId)) Map.empty[AID, Auction.State]
        else Map(auctionId -> UIJDLattice[Auction.State].bottom)

      deltaState(auctions = Some(newMap))
  }

  def requestRegisterUser[C: CContext](userId: User): DeltaMutator[State[C]] = {
    case (replicaID, (req, users, _)) =>
      if (users.contains(userId)) deltaState()
      else deltaState(userRequests = Some(AWSetCRDT.add(userId -> replicaID).apply(replicaID, req)))
  }

  def resolveRegisterUser[C: CContext](): DeltaMutator[State[C]] = {
    case (replicaID, (req, users, _)) =>
      val newUsers = AWSetCRDT.elements[(User, String), C].apply(req).foldLeft(Map.empty[User, String]) {
        case (newlyRegistered, (uid, rid)) =>
          if ((users ++ newlyRegistered).contains(uid))
            newlyRegistered
          else {
            newlyRegistered.updated(uid, rid)
          }
      }

      deltaState(
        userRequests = Some(AWSetCRDT.clear[(User, String), C]().apply(replicaID, req)),
        users = Some(newUsers)
      )
  }
}

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

  implicit def RubisStateCodec[C: JsonValueCodec]: JsonValueCodec[(
      Causal[Map[(String, String), Set[Dot]], C],
      Map[String, String],
      Map[String, AuctionData]
  )] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
}
