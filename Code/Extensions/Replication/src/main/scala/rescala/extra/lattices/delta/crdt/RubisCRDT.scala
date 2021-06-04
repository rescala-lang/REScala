package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta._
import rescala.extra.lattices.delta.crdt.Bid.User

object RubisCRDT {
  type AID = String

  type State[C] = (AWSetCRDT.State[(User, String), C], Map[User, String], Map[AID, AuctionCRDT.State])

  implicit val UserAsUIJDLattice: UIJDLattice[User] = UIJDLattice.AtomicUIJDLattice[User]

  private def deltaState[C: CContext](
      userRequests: Option[AWSetCRDT.State[(User, String), C]] = None,
      users: Option[Map[User, String]] = None,
      auctions: Option[Map[AID, AuctionCRDT.State]] = None
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
        } else Map.empty[AID, AuctionCRDT.State]

      deltaState(auctions = Some(newMap))
  }

  def closeAuction[C: CContext](auctionId: AID): DeltaMutator[State[C]] = {
    case (replicaID, (_, _, m)) =>
      val newMap =
        if (m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => AuctionCRDT.close()(replicaID, a)) }
        } else Map.empty[AID, AuctionCRDT.State]

      deltaState(auctions = Some(newMap))
  }

  def openAuction[C: CContext](auctionId: AID): DeltaMutator[State[C]] = {
    case (_, (_, _, m)) =>
      val newMap =
        if (m.contains(auctionId)) Map.empty[AID, AuctionCRDT.State]
        else Map(auctionId -> UIJDLattice[AuctionCRDT.State].bottom)

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
