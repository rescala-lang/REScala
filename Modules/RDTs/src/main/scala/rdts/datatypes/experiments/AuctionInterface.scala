package rdts.datatypes.experiments

import rdts.base.{Bottom, Decompose, Lattice}
import rdts.datatypes.experiments.AuctionInterface.Bid.User
import rdts.dotted.HasDots

object AuctionInterface {
  sealed trait Status
  case object Open   extends Status
  case object Closed extends Status

  object Status {

    given ordering: Ordering[Status] with {
      override def compare(x: Status, y: Status): Int = if x == y then 0 else if x == Closed then 1 else -1
    }

    given lattice: Lattice[Status] = Lattice.fromOrdering
  }

  case class Bid(userId: User, bid: Int)

  object Bid {
    type User = String
  }

  case class AuctionData(
      bids: Set[Bid] = Set.empty,
      status: Status = Open,
      winner: Option[User] = None
  ) {
    def bid(userId: User, price: Int): AuctionData =
      AuctionData(bids = Set(Bid(userId, price)))

    def knockDown(): AuctionData = AuctionData(status = Closed)

  }

  object AuctionData {

    val empty: AuctionData = AuctionData()

    given bottom: Bottom[AuctionData] with { override def empty: AuctionData = AuctionData.empty }
    given hasDots: HasDots[AuctionData] = HasDots.noDots

    given AuctionDataAsUIJDLattice: Lattice[AuctionData] with Decompose[AuctionData] with {
      override def lteq(left: AuctionData, right: AuctionData): Boolean = (left, right) match {
        case (AuctionData(lb, ls, _), AuctionData(rb, rs, _)) =>
          Lattice[Set[Bid]].lteq(lb, rb) && Lattice[Status].lteq(ls, rs)
      }

      extension (a: AuctionData)
        override def decomposed: Iterable[AuctionData] = a match {
          case AuctionData(bids, status, _) =>
            bids.map(b =>
              AuctionData(bids = Set(b))
            ) ++ (status match {
              case Open   => Set()
              case Closed => Set(AuctionData(status = Closed))
            })
        }

      override def merge(left: AuctionData, right: AuctionData): AuctionData = (left, right) match {
        case (AuctionData(lb, ls, _), AuctionData(rb, rs, _)) =>
          val bidsMerged   = Lattice[Set[Bid]].merge(lb, rb)
          val statusMerged = Lattice[Status].merge(ls, rs)
          val winnerMerged = statusMerged match {
            case Open   => None
            case Closed => bidsMerged.maxByOption(_.bid).map(_.userId)
          }

          AuctionData(bidsMerged, statusMerged, winnerMerged)
      }
    }

  }
}
