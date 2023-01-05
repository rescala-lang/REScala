package kofre.protocol

import kofre.base.{Bottom, DecomposeLattice}
import kofre.decompose.*
import kofre.datatypes.AddWinsSet.AWSetSyntax
import kofre.datatypes.AddWinsSet
import kofre.dotted.{DottedDecompose, Dotted}
import kofre.protocol.AuctionInterface
import kofre.protocol.AuctionInterface.Bid.User
import kofre.syntax.{OpsSyntaxHelper, PermIdMutate}
import kofre.base.Id

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
object RubisInterface {
  type AID = String

  type State = (AddWinsSet[(User, Id)], Map[User, Id], Map[AID, AuctionInterface.AuctionData])

  private class DeltaStateFactory {
    val bottom: State = (AddWinsSet.empty, Map.empty, Map.empty)

    def make(
        userRequests: AddWinsSet[(User, Id)] = bottom._1,
        users: Map[User, Id] = bottom._2,
        auctions: Map[AID, AuctionInterface.AuctionData] = bottom._3
    ): State = (userRequests, users, auctions)
  }

  private def deltaState: DeltaStateFactory = new DeltaStateFactory

  implicit class RubisSyntax[C](container: C) extends OpsSyntaxHelper[C, State](container) {

    def placeBid(auctionId: AID, userId: User, price: Int)(using MutationIdP): C = {
      val (_, users, m) = current
      val newMap =
        if (users.get(userId).contains(replicaID) && m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => a.bid(userId, price)) }
        } else Map.empty[AID, AuctionInterface.AuctionData]

      deltaState.make(auctions = newMap).mutator
    }

    def closeAuction(auctionId: AID)(using MutationIdP): C = {
      val (_, _, m) = current
      val newMap =
        if (m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => a.close()) }
        } else Map.empty[AID, AuctionInterface.AuctionData]

      deltaState.make(auctions = newMap).mutator
    }

    def openAuction(auctionId: AID)(using MutationIdP): C = {
      val (_, _, m) = current
      val newMap =
        if (m.contains(auctionId)) Map.empty[AID, AuctionInterface.AuctionData]
        else Map(auctionId -> AuctionInterface.AuctionData())

      deltaState.make(auctions = newMap).mutator
    }

    def requestRegisterUser(userId: User)(using CausalMutationP, CausalP, QueryP, IdentifierP): C = {
      val (req, users, _) = current
      if (users.contains(userId)) Dotted(deltaState.make(), context).mutator
      else
        val merged = Dotted(req, context).named(replicaID).add(userId -> replicaID).anon
        Dotted(deltaState.make(userRequests = merged.store), merged.context).mutator
    }

    def resolveRegisterUser()(using MutationIdP, CausalP): C = {
      val (req, users, _) = current
      val newUsers = req.elements.foldLeft(Map.empty[User, Id]) {
        case (newlyRegistered, (uid, rid)) =>
          if ((users ++ newlyRegistered).contains(uid))
            newlyRegistered
          else {
            newlyRegistered.updated(uid, rid)
          }
      }

      deltaState.make(
        userRequests = Dotted(req, context).clear().store,
        users = newUsers
      )
    }.mutator
  }
}
