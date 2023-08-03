package test.kofre.bespoke

import kofre.datatypes.experiments.AuctionInterface
import kofre.datatypes.experiments.AuctionInterface.{AuctionData, Bid}
import kofre.dotted.Dotted
import kofre.syntax.{DeltaBuffer, DeltaBufferContainer}

class DottedTest extends munit.FunSuite {

  test("AuctionData can be in Dotted") {
    val auction: Dotted[AuctionData] = Dotted.empty

    assert(auction.data.bids   == Set.empty)
    assert(auction.data.status == AuctionInterface.Open)
    assert(auction.data.winner == None)

    val added = auction.bid("First", 1)
    assert(added.data.bids   == Set(Bid("First", 1)))
    assert(added.data.status == AuctionInterface.Open)
    assert(added.data.winner == None)

    val closed = added merge added.close()
    assert(closed.data.bids   == Set(Bid("First", 1)))
    assert(closed.data.status == AuctionInterface.Closed)
    assert(closed.data.winner == Some("First"))
  }

  test("AuctionData can be in DeltaBuffer") {
    val auction: DeltaBuffer[AuctionData] = DeltaBuffer(AuctionData.empty)

    assert(auction.state.bids   == Set.empty)
    assert(auction.state.status == AuctionInterface.Open)
    assert(auction.state.winner == None)

    val added = auction.bid("First", 1)
    assert(added.state.bids   == Set(Bid("First", 1)))
    assert(added.state.status == AuctionInterface.Open)
    assert(added.state.winner == None)

    val closed = added.close()
    assert(closed.state.bids   == Set(Bid("First", 1)))
    assert(closed.state.status == AuctionInterface.Closed)
    assert(closed.state.winner == Some("First"))
  }

  test("Dotted[AuctionData] can be in DeltaBuffer") {
    val auction: DeltaBuffer[Dotted[AuctionData]] = DeltaBuffer(Dotted.empty)

    assert(auction.state.data.bids   == Set.empty)
    assert(auction.state.data.status == AuctionInterface.Open)
    assert(auction.state.data.winner == None)

    val added = auction.bid("First", 1)
    assert(added.state.data.bids   == Set(Bid("First", 1)))
    assert(added.state.data.status == AuctionInterface.Open)
    assert(added.state.data.winner == None)

    val closed = added.close()
    assert(closed.state.data.bids   == Set(Bid("First", 1)))
    assert(closed.state.data.status == AuctionInterface.Closed)
    assert(closed.state.data.winner == Some("First"))
  }

  test("AuctionData can be in DeltaBufferContainer") {
    val auction: DeltaBufferContainer[AuctionData] = DeltaBufferContainer(DeltaBuffer(AuctionData.empty))

    assert(auction.result.state.bids == Set.empty)
    assert(auction.result.state.status == AuctionInterface.Open)
    assert(auction.result.state.winner == None)

    val added = auction.bid("First", 1)
    assert(added.result.state.bids == Set(Bid("First", 1)))
    assert(added.result.state.status == AuctionInterface.Open)
    assert(added.result.state.winner == None)

    val closed = added.close()
    assert(closed.result.state.bids == Set(Bid("First", 1)))
    assert(closed.result.state.status == AuctionInterface.Closed)
    assert(closed.result.state.winner == Some("First"))
  }

  test("Dotted[AuctionData] can be in DeltaBufferContainer") {
    val auction: DeltaBufferContainer[Dotted[AuctionData]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assert(auction.result.state.data.bids   == Set.empty)
    assert(auction.result.state.data.status == AuctionInterface.Open)
    assert(auction.result.state.data.winner == None)

    val added = auction.bid("First", 1)
    assert(added.result.state.data.bids   == Set(Bid("First", 1)))
    assert(added.result.state.data.status == AuctionInterface.Open)
    assert(added.result.state.data.winner == None)

    val closed = added.close()
    assert(closed.result.state.data.bids   == Set(Bid("First", 1)))
    assert(closed.result.state.data.status == AuctionInterface.Closed)
    assert(closed.result.state.data.winner == Some("First"))
  }

}
