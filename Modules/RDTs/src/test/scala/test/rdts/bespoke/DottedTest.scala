package test.rdts.bespoke

import rdts.datatypes.experiments.AuctionInterface
import rdts.datatypes.experiments.AuctionInterface.{AuctionData, Bid}
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, DeltaBufferContainer}

class DottedTest extends munit.FunSuite {

  test("AuctionData can be in Dotted") {
    val auction: Dotted[AuctionData] = Dotted.empty

    assert(auction.data.bids == Set.empty)
    assert(auction.data.status == AuctionInterface.Open)
    assert(auction.data.winner == None)

    val added = auction.map(_.bid("First", 1))
    assert(added.data.bids == Set(Bid("First", 1)))
    assert(added.data.status == AuctionInterface.Open)
    assert(added.data.winner == None)

    val knockedDown = added merge added.map(_.knockDown())
    assert(knockedDown.data.bids == Set(Bid("First", 1)))
    assert(knockedDown.data.status == AuctionInterface.Closed)
    assert(knockedDown.data.winner == Some("First"))
  }

  test("AuctionData can be in DeltaBuffer") {
    val auction: DeltaBuffer[AuctionData] = DeltaBuffer(AuctionData.empty)

    assert(auction.state.bids == Set.empty)
    assert(auction.state.status == AuctionInterface.Open)
    assert(auction.state.winner == None)

    val added = auction.modp(_.bid("First", 1))
    assert(added.state.bids == Set(Bid("First", 1)))
    assert(added.state.status == AuctionInterface.Open)
    assert(added.state.winner == None)

    val knockedDown = added.modp(_.knockDown())
    assert(knockedDown.state.bids == Set(Bid("First", 1)))
    assert(knockedDown.state.status == AuctionInterface.Closed)
    assert(knockedDown.state.winner == Some("First"))
  }

  test("Dotted[AuctionData] can be in DeltaBuffer") {
    val auction: DeltaBuffer[Dotted[AuctionData]] = DeltaBuffer(Dotted.empty)

    assert(auction.state.data.bids == Set.empty)
    assert(auction.state.data.status == AuctionInterface.Open)
    assert(auction.state.data.winner == None)

    val added = auction.modn(_.bid("First", 1))
    assert(added.state.data.bids == Set(Bid("First", 1)))
    assert(added.state.data.status == AuctionInterface.Open)
    assert(added.state.data.winner == None)

    val knockedDown = added.modn(_.knockDown())
    assert(knockedDown.state.data.bids == Set(Bid("First", 1)))
    assert(knockedDown.state.data.status == AuctionInterface.Closed)
    assert(knockedDown.state.data.winner == Some("First"))
  }

  test("AuctionData can be in DeltaBufferContainer") {
    val auction: DeltaBufferContainer[AuctionData] = DeltaBufferContainer(DeltaBuffer(AuctionData.empty))

    assert(auction.result.state.bids == Set.empty)
    assert(auction.result.state.status == AuctionInterface.Open)
    assert(auction.result.state.winner == None)

    val added = auction.modp(_.bid("First", 1))
    assert(added.result.state.bids == Set(Bid("First", 1)))
    assert(added.result.state.status == AuctionInterface.Open)
    assert(added.result.state.winner == None)

    val knockedDown = added.modp(_.knockDown())
    assert(knockedDown.result.state.bids == Set(Bid("First", 1)))
    assert(knockedDown.result.state.status == AuctionInterface.Closed)
    assert(knockedDown.result.state.winner == Some("First"))
  }

  test("Dotted[AuctionData] can be in DeltaBufferContainer") {
    val auction: DeltaBufferContainer[Dotted[AuctionData]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assert(auction.result.state.data.bids == Set.empty)
    assert(auction.result.state.data.status == AuctionInterface.Open)
    assert(auction.result.state.data.winner == None)

    val added = auction.modn(_.bid("First", 1))
    assert(added.result.state.data.bids == Set(Bid("First", 1)))
    assert(added.result.state.data.status == AuctionInterface.Open)
    assert(added.result.state.data.winner == None)

    val knockedDown = added.modn(_.knockDown())
    assert(knockedDown.result.state.data.bids == Set(Bid("First", 1)))
    assert(knockedDown.result.state.data.status == AuctionInterface.Closed)
    assert(knockedDown.result.state.data.winner == Some("First"))
  }

}
