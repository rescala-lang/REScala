package kofre.protocol

import kofre.base.{Bottom, DecomposeLattice}
import kofre.contextual.{ContextDecompose, WithContext}
import kofre.decompose.*
import kofre.predef.AddWinsSet.AWSetSyntax
import kofre.predef.AddWinsSet
import kofre.protocol.AuctionInterface
import kofre.protocol.AuctionInterface.Bid.User
import kofre.syntax.{OpsSyntaxHelper, PermIdMutate}

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

  type State = (AddWinsSet[(User, String)], Map[User, String], Map[AID, AuctionInterface.AuctionData])

  private class DeltaStateFactory {
    val bottom: State = (AddWinsSet.empty, Map.empty, Map.empty)

    def make(
        userRequests: AddWinsSet[(User, String)] = bottom._1,
        users: Map[User, String] = bottom._2,
        auctions: Map[AID, AuctionInterface.AuctionData] = bottom._3
    ): State = (userRequests, users, auctions)
  }

  private def deltaState: DeltaStateFactory = new DeltaStateFactory

  implicit class RubisSyntax[C](container: C) extends OpsSyntaxHelper[C, State](container) {

    def placeBid(auctionId: AID, userId: User, price: Int)(using MutationIDP): C = {
      val (_, users, m) = current
      val newMap =
        if (users.get(userId).contains(replicaID) && m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => a.bid(userId, price)) }
        } else Map.empty[AID, AuctionInterface.AuctionData]

      deltaState.make(auctions = newMap)
    }

    def closeAuction(auctionId: AID)(using MutationIDP): C = {
      val (_, _, m) = current
      val newMap =
        if (m.contains(auctionId)) {
          m.updatedWith(auctionId) { _.map(a => a.close()) }
        } else Map.empty[AID, AuctionInterface.AuctionData]

      deltaState.make(auctions = newMap)
    }

    def openAuction(auctionId: AID)(using MutationIDP): C = {
      val (_, _, m) = current
      val newMap =
        if (m.contains(auctionId)) Map.empty[AID, AuctionInterface.AuctionData]
        else Map(auctionId -> DecomposeLattice[AuctionInterface.AuctionData].empty)

      deltaState.make(auctions = newMap)
    }

    def requestRegisterUser(userId: User)(using CausalMutationP, CausalP, QueryP, IdentifierP): C = {
      val (req, users, _) = current
      if (users.contains(userId)) WithContext(deltaState.make(), context).mutator
      else
        val merged = WithContext(req, context).named(replicaID).add(userId -> replicaID).inner
        WithContext(deltaState.make(userRequests = merged.store), merged.context).mutator
    }

    def resolveRegisterUser()(using MutationIDP, CausalP): C = {
      val (req, users, _) = current
      val newUsers = req.elements.foldLeft(Map.empty[User, String]) {
        case (newlyRegistered, (uid, rid)) =>
          if ((users ++ newlyRegistered).contains(uid))
            newlyRegistered
          else {
            newlyRegistered.updated(uid, rid)
          }
      }

      deltaState.make(
        userRequests = WithContext(req, context).clear().store,
        users = newUsers
      )
    }
  }
}
