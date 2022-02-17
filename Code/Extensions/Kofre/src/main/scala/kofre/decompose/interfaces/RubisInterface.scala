package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.decompose.CRDTInterface.DeltaMutator
import kofre.decompose.interfaces.AuctionInterface.Bid.User
import kofre.decompose.interfaces.RubisInterface.{AID, UserAsUIJDLattice}

object RubisInterface {
  type AID = String

  type State = (AWSetInterface.State[(User, String)], Map[User, String], Map[AID, AuctionInterface.State])

  trait RubisCompanion {
    type State = RubisInterface.State

    implicit val UserAsUIJDLattice: UIJDLattice[User] = RubisInterface.UserAsUIJDLattice
  }

  implicit val UserAsUIJDLattice: UIJDLattice[User] = UIJDLattice.AtomicUIJDLattice[User]

  private class DeltaStateFactory {
    val bottom: State = UIJDLattice[State].bottom

    def make(
        userRequests: AWSetInterface.State[(User, String)] = bottom._1,
        users: Map[User, String] = bottom._2,
        auctions: Map[AID, AuctionInterface.State] = bottom._3
    ): State = (userRequests, users, auctions)
  }

  private def deltaState: DeltaStateFactory = new DeltaStateFactory

  def placeBid(auctionId: AID, userId: User, price: Int): DeltaMutator[State] = {
    case (replicaID, (_, users, m)) =>
      val newMap =
        if (users.get(userId).contains(replicaID) && m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => AuctionInterface.bid(userId, price)(replicaID, a)) }
        } else Map.empty[AID, AuctionInterface.State]

      deltaState.make(auctions = newMap)
  }

  def closeAuction(auctionId: AID): DeltaMutator[State] = {
    case (replicaID, (_, _, m)) =>
      val newMap =
        if (m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => AuctionInterface.close()(replicaID, a)) }
        } else Map.empty[AID, AuctionInterface.State]

      deltaState.make(auctions = newMap)
  }

  def openAuction(auctionId: AID): DeltaMutator[State] = {
    case (_, (_, _, m)) =>
      val newMap =
        if (m.contains(auctionId)) Map.empty[AID, AuctionInterface.State]
        else Map(auctionId -> UIJDLattice[AuctionInterface.State].bottom)

      deltaState.make(auctions = newMap)
  }

  def requestRegisterUser(userId: User): DeltaMutator[State] = {
    case (replicaID, (req, users, _)) =>
      if (users.contains(userId)) deltaState.make()
      else deltaState.make(userRequests = AWSetInterface.add(userId -> replicaID).apply(replicaID, req))
  }

  def resolveRegisterUser(): DeltaMutator[State] = {
    case (replicaID, (req, users, _)) =>
      val newUsers = AWSetInterface.elements[(User, String)].apply(req).foldLeft(Map.empty[User, String]) {
        case (newlyRegistered, (uid, rid)) =>
          if ((users ++ newlyRegistered).contains(uid))
            newlyRegistered
          else {
            newlyRegistered.updated(uid, rid)
          }
      }

      deltaState.make(
        userRequests = AWSetInterface.clear[(User, String)]().apply(replicaID, req),
        users = newUsers
      )
  }
}

/** A Rubis (Rice University Bidding System) is a Delta CRDT modeling an auction system.
  *
  * Bids can only be placed on auctions that were previously opened and with a previously registered userId. When an auction
  * is closed, concurrently placed bids are still accepted and may thus change the winner of the auction. To prevent two
  * replicas from concurrently registering the same userId, requests for registering a new userId must be resolved by a
  * central replica using resolveRegisterUser.
  *
  * This auction system was in part modeled after the Rice University Bidding System (RUBiS) proposed by Cecchet et al. in
  * "Performance and Scalability of EJB Applications", see [[https://www.researchgate.net/publication/2534515_Performance_and_Scalability_of_EJB_Applications here]]
  */
abstract class RubisInterface[ Wrapper] extends CRDTInterface[RubisInterface.State, Wrapper] {
  def placeBid(auctionId: AID, userId: User, price: Int): Wrapper =
    mutate(RubisInterface.placeBid(auctionId, userId, price))

  def closeAuction(auctionId: AID): Wrapper = mutate(RubisInterface.closeAuction(auctionId))

  def openAuction(auctionId: AID): Wrapper = mutate(RubisInterface.openAuction(auctionId))

  def requestRegisterUser(userId: User): Wrapper = mutate(RubisInterface.requestRegisterUser(userId))

  def resolveRegisterUser(): Wrapper = mutate(RubisInterface.resolveRegisterUser())

  def printState(): Unit = println(state)
}
