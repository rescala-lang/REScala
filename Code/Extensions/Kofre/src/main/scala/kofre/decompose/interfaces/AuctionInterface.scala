package kofre.decompose.interfaces

import kofre.decompose.*
import kofre.decompose.CRDTInterface.DeltaMutator
import kofre.decompose.interfaces.AuctionInterface.Bid.User

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
      bids: GSetInterface.State[Bid] = UIJDLattice[GSetInterface.State[Bid]].bottom,
      status: Status = UIJDLattice[Status].bottom,
      winner: Option[User] = None
  )

  case object AuctionData {
    implicit val AuctionDataAsUIJDLattice: UIJDLattice[AuctionData] = new UIJDLattice[AuctionData] {
      override def leq(left: AuctionData, right: AuctionData): Boolean = (left, right) match {
        case (AuctionData(lb, ls, _), AuctionData(rb, rs, _)) =>
          UIJDLattice[GSetInterface.State[Bid]].leq(lb, rb) && UIJDLattice[Status].leq(ls, rs)
      }

      override def decompose(state: AuctionData): Iterable[AuctionInterface.State] =
        state match {
          case AuctionData(bids, status, _) =>
            bids.map(b =>
              AuctionData(bids = GSetInterface.insert(b)("", UIJDLattice[GSetInterface.State[Bid]].bottom))
            ) ++ (status match {
              case Open   => Set()
              case Closed => Set(AuctionData(status = Closed))
            })
        }

      override def bottom: AuctionData = AuctionData()

      override def merge(left: AuctionData, right: AuctionData): AuctionData = (left, right) match {
        case (AuctionData(lb, ls, _), AuctionData(rb, rs, _)) =>
          val bidsMerged   = UIJDLattice[GSetInterface.State[Bid]].merge(lb, rb)
          val statusMerged = UIJDLattice[Status].merge(ls, rs)
          val winnerMerged = statusMerged match {
            case Open   => None
            case Closed => bidsMerged.maxByOption(_.bid).map(_.userId)
          }

          AuctionData(bidsMerged, statusMerged, winnerMerged)
      }
    }
  }

  type State = AuctionData

  def bid(userId: User, price: Int): DeltaMutator[State] = {
    case (replicaID, AuctionData(bids, _, _)) =>
      AuctionData(bids = GSetInterface.insert(Bid(userId, price))(replicaID, bids))
  }

  def close(): DeltaMutator[State] = (_, _) => AuctionData(status = Closed)
}
