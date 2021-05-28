package rescala.extra.lattices.delta.crdt

import rescala.extra.lattices.delta.DeltaCRDT.DeltaMutator
import rescala.extra.lattices.delta.UIJDLattice
import rescala.extra.lattices.delta.crdt.Bid.User

sealed trait Status
case object Open   extends Status
case object Closed extends Status

object Status {
  implicit val StatusAsUIJDLattice: UIJDLattice[Status] = new UIJDLattice[Status] {
    override def leq(left: Status, right: Status): Boolean = (left, right) match {
      case (Closed, Open) => false
      case _              => true
    }

    override def decompose(state: Status): Set[Status] = Set(state)

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
    bids: GSet.State[Bid] = UIJDLattice[GSet.State[Bid]].bottom,
    status: Status = UIJDLattice[Status].bottom,
    winner: Option[User] = None
)

case object AuctionData {
  implicit val AuctionDataAsUIJDLattice: UIJDLattice[AuctionData] = new UIJDLattice[AuctionData] {
    override def leq(left: AuctionData, right: AuctionData): Boolean = (left, right) match {
      case (AuctionData(lb, ls, _), AuctionData(rb, rs, _)) =>
        UIJDLattice[GSet.State[Bid]].leq(lb, rb) && UIJDLattice[Status].leq(ls, rs)
    }

    override def decompose(state: AuctionData): Set[AuctionData] = state match {
      case AuctionData(bids, status, _) =>
        bids.map(b =>
          AuctionData(bids = GSetCRDT.insert(b)("", UIJDLattice[GSet.State[Bid]].bottom))
        ) ++ (status match {
          case Open   => Set()
          case Closed => Set(AuctionData(status = Closed))
        })
    }

    override def bottom: AuctionData = AuctionData()

    override def merge(left: AuctionData, right: AuctionData): AuctionData = (left, right) match {
      case (AuctionData(lb, ls, _), AuctionData(rb, rs, _)) =>
        val bidsMerged   = UIJDLattice[GSet.State[Bid]].merge(lb, rb)
        val statusMerged = UIJDLattice[Status].merge(ls, rs)
        val winnerMerged = statusMerged match {
          case Open   => None
          case Closed => bidsMerged.maxByOption(_.bid).map(_.userId)
        }

        AuctionData(bidsMerged, statusMerged, winnerMerged)
    }
  }
}

object AuctionCRDT {
  type State = AuctionData

  def bid(userId: User, price: Int): DeltaMutator[State] = {
    case (replicaID, AuctionData(bids, _, _)) =>
      AuctionData(bids = GSetCRDT.insert(Bid(userId, price))(replicaID, bids))
  }

  def close(): DeltaMutator[State] = (_, _) => AuctionData(status = Closed)
}

object Auction {
  type State = AuctionCRDT.State
}
