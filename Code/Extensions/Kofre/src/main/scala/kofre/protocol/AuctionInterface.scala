package kofre.protocol

import kofre.decompose.*
import kofre.decompose.interfaces.EWFlagInterface.EWFlag
import kofre.decompose.interfaces.GSetInterface
import kofre.decompose.interfaces.GSetInterface.GSetSyntax
import kofre.protocol.AuctionInterface.Bid.User
import kofre.syntax.OpsSyntaxHelper

object AuctionInterface {
  sealed trait Status
  case object Open   extends Status
  case object Closed extends Status

  object Status {
    implicit val StatusAsUIJDLattice: UIJDLattice[Status] = new UIJDLattice[Status] {
      override def leq(left: Status, right: Status): Boolean = (left, right) match {
        case (Closed, Open) => false
        case _              => true
      }

      override def decompose(state: Status): Iterable[Status] = List(state)

      override def bottom: Status = Open

      override def merge(left: Status, right: Status): Status = (left, right) match {
        case (Open, Open) => Open
        case _            => Closed
      }
    }
  }

  case class Bid(userId: User, bid: Int)

  case object Bid {
    type User = String
  }

  case class AuctionData(
      bids: Set[Bid] = UIJDLattice[Set[Bid]].bottom,
      status: Status = UIJDLattice[Status].bottom,
      winner: Option[User] = None
  )

  case object AuctionData {
    implicit val AuctionDataAsUIJDLattice: UIJDLattice[AuctionData] = new UIJDLattice[AuctionData] {
      override def leq(left: AuctionData, right: AuctionData): Boolean = (left, right) match {
        case (AuctionData(lb, ls, _), AuctionData(rb, rs, _)) =>
          UIJDLattice[Set[Bid]].leq(lb, rb) && UIJDLattice[Status].leq(ls, rs)
      }

      override def decompose(state: AuctionData): Iterable[AuctionData] =
        state match {
          case AuctionData(bids, status, _) =>
            bids.map(b =>
              AuctionData(bids = UIJDLattice[Set[Bid]].bottom.insert(b))
            ) ++ (status match {
              case Open   => Set()
              case Closed => Set(AuctionData(status = Closed))
            })
        }

      override def bottom: AuctionData = AuctionData()

      override def merge(left: AuctionData, right: AuctionData): AuctionData = (left, right) match {
        case (AuctionData(lb, ls, _), AuctionData(rb, rs, _)) =>
          val bidsMerged   = UIJDLattice[Set[Bid]].merge(lb, rb)
          val statusMerged = UIJDLattice[Status].merge(ls, rs)
          val winnerMerged = statusMerged match {
            case Open   => None
            case Closed => bidsMerged.maxByOption(_.bid).map(_.userId)
          }

          AuctionData(bidsMerged, statusMerged, winnerMerged)
      }
    }
  }

  implicit class AuctionSyntax[C](container: C) extends OpsSyntaxHelper[C, AuctionData](container) {
    def bid(userId: User, price: Int)(using MutationP): C =
      AuctionData(bids = current.bids.insert(Bid(userId, price)))

    def close()(using MutationP): C = AuctionData(status = Closed)
  }
}
