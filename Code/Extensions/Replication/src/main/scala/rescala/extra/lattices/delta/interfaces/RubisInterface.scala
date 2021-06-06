package rescala.extra.lattices.delta.interfaces

import rescala.extra.lattices.delta.CRDTInterface.DeltaMutator
import rescala.extra.lattices.delta._
import rescala.extra.lattices.delta.interfaces.AuctionInterface.Bid.User
import rescala.extra.lattices.delta.interfaces.RubisInterface.{AID, UserAsUIJDLattice}

object RubisInterface {
  type AID = String

  type State[C] = (AWSetInterface.State[(User, String), C], Map[User, String], Map[AID, AuctionInterface.State])

  trait RubisCompanion {
    type State[C] = RubisInterface.State[C]

    implicit val UserAsUIJDLattice: UIJDLattice[User] = RubisInterface.UserAsUIJDLattice
  }

  implicit val UserAsUIJDLattice: UIJDLattice[User] = UIJDLattice.AtomicUIJDLattice[User]

  private def deltaState[C: CContext](
      userRequests: Option[AWSetInterface.State[(User, String), C]] = None,
      users: Option[Map[User, String]] = None,
      auctions: Option[Map[AID, AuctionInterface.State]] = None
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
          m.updatedWith(auctionId) { _.map(a => AuctionInterface.bid(userId, price)(replicaID, a)) }
        } else Map.empty[AID, AuctionInterface.State]

      deltaState(auctions = Some(newMap))
  }

  def closeAuction[C: CContext](auctionId: AID): DeltaMutator[State[C]] = {
    case (replicaID, (_, _, m)) =>
      val newMap =
        if (m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => AuctionInterface.close()(replicaID, a)) }
        } else Map.empty[AID, AuctionInterface.State]

      deltaState(auctions = Some(newMap))
  }

  def openAuction[C: CContext](auctionId: AID): DeltaMutator[State[C]] = {
    case (_, (_, _, m)) =>
      val newMap =
        if (m.contains(auctionId)) Map.empty[AID, AuctionInterface.State]
        else Map(auctionId -> UIJDLattice[AuctionInterface.State].bottom)

      deltaState(auctions = Some(newMap))
  }

  def requestRegisterUser[C: CContext](userId: User): DeltaMutator[State[C]] = {
    case (replicaID, (req, users, _)) =>
      if (users.contains(userId)) deltaState()
      else deltaState(userRequests = Some(AWSetInterface.add(userId -> replicaID).apply(replicaID, req)))
  }

  def resolveRegisterUser[C: CContext](): DeltaMutator[State[C]] = {
    case (replicaID, (req, users, _)) =>
      val newUsers = AWSetInterface.elements[(User, String), C].apply(req).foldLeft(Map.empty[User, String]) {
        case (newlyRegistered, (uid, rid)) =>
          if ((users ++ newlyRegistered).contains(uid))
            newlyRegistered
          else {
            newlyRegistered.updated(uid, rid)
          }
      }

      deltaState(
        userRequests = Some(AWSetInterface.clear[(User, String), C]().apply(replicaID, req)),
        users = Some(newUsers)
      )
  }
}

abstract class RubisInterface[C: CContext, Wrapper] extends CRDTInterface[RubisInterface.State[C], Wrapper] {
  def placeBid(auctionId: AID, userId: User, price: Int): Wrapper =
    mutate(RubisInterface.placeBid(auctionId, userId, price))

  def closeAuction(auctionId: AID): Wrapper = mutate(RubisInterface.closeAuction(auctionId))

  def openAuction(auctionId: AID): Wrapper = mutate(RubisInterface.openAuction(auctionId))

  def requestRegisterUser(userId: User): Wrapper = mutate(RubisInterface.requestRegisterUser(userId))

  def resolveRegisterUser(): Wrapper = mutate(RubisInterface.resolveRegisterUser())

  def printState(): Unit = println(state)
}
