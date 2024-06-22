package test.rdts.containers

import rdts.base.Bottom
import rdts.base.Uid.asId
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.{EnableWinsFlag, ReplicatedSet}
import rdts.datatypes.experiments.AuctionInterface
import rdts.datatypes.experiments.AuctionInterface.{AuctionData, Bid}
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, DeltaBufferContainer, LocalUid}
import test.rdts.UtilHacks.*
import test.rdts.UtilHacks2.*

class ContainerTest extends munit.FunSuite {

  object helper {

    given r: LocalUid = "me".asId

    given bottomString: Bottom[String] with {
      override def empty: String = ""
    }

  }

  import helper.given

  // START EnableWinsFlag

  test("Dotted can contain contextual EnableWinsFlag") {
    val flag: Dotted[EnableWinsFlag] = Dotted.empty
    given LocalUid                   = "me".asId

    assertEquals(flag.data.read, false)

    val enabled = flag.mod(_.enable())
    assertEquals(enabled.data.read, true)

    val disabled = enabled.mod(_.disable())
    assertEquals(disabled.data.read, false)
  }

  // NOTE: DeltaBuffer cannot contain contextual EnableWinsFlag without Dotted, as EnableWinsFlag needs a context

  test("Dotted DeltaBuffer can contain contextual EnableWinsFlag") {
    val flag: DeltaBuffer[Dotted[EnableWinsFlag]] = DeltaBuffer(Dotted.empty)

    assertEquals(flag.data.read, false)

    val enabled = flag.modd(_.enable())
    assertEquals(enabled.data.read, true)

    val disabled = enabled.modd(_.disable())
    assertEquals(disabled.data.read, false)
  }

  test("Dotted DeltaBufferContainer can contain contextual EnableWinsFlag") {
    val flag: DeltaBufferContainer[Dotted[EnableWinsFlag]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assertEquals(flag.data.read, false)

    flag.mod(_.enable())
    assertEquals(flag.data.read, true)

    flag.mod(_.disable())
    assertEquals(flag.data.read, false)
  }

  // END EnableWinsFlag

  // START ReplicatedSet

  test("Dotted can contain contextual ReplicatedSet[String]") {
    val awSet: Dotted[ReplicatedSet[String]] = Dotted.empty

    assert(awSet.data.elements.isEmpty)

    val added = awSet.mod(_.add("First"))
    assertEquals(added.data.elements.size, 1)
    assert(added.data.elements.contains("First"))
    assert(added.data.contains("First"))

    val removed = added.mod(_.remove("First"))
    assert(removed.data.elements.isEmpty)
  }

  // NOTE: DeltaBuffer cannot contain contextual ReplicatedSet without Dotted, as ReplicatedSet needs a context

  test("Dotted DeltaBuffer can contain contextual ReplicatedSet[String]") {
    val awSet: DeltaBuffer[Dotted[ReplicatedSet[String]]] = DeltaBuffer(Dotted.empty)

    assert(awSet.data.elements.isEmpty)

    val added = awSet.modd(_.add("First"))
    assertEquals(added.data.elements.size, 1)
    assert(added.data.elements.contains("First"))
    assert(added.data.contains("First"))

    val removed = added.modd(_.remove("First"))
    assert(removed.data.elements.isEmpty)
  }

  test("Dotted DeltaBufferContainer can contain contextual ReplicatedSet[String]") {
    val awSet: DeltaBufferContainer[Dotted[ReplicatedSet[String]]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assert(awSet.data.elements.isEmpty)

    awSet.mod(_.add("First"))
    assertEquals(awSet.data.elements.size, 1)
    assert(awSet.data.elements.contains("First"))
    assert(awSet.data.contains("First"))

    awSet.mod(_.remove("First"))
    assert(awSet.data.elements.isEmpty)
  }

  // END ReplicatedSet

  // START LastWriterWins

  test("Dotted can contain non-contextual LastWriterWins[String]") {
    val lww: Dotted[LastWriterWins[String]] = Dotted.empty

    assertEquals(lww.data.read, "")

    val added = lww.write("First")
    assertEquals(added.data.read, "First")

    val removed = added.write("")
    assertEquals(removed.data.read, "")
  }

  test("DeltaBuffer can contain non-contextual LastWriterWins[String]") {
    val lww: DeltaBuffer[LastWriterWins[String]] = DeltaBuffer(LastWriterWins.empty)

    assertEquals(lww.read, "")

    val added = lww.write("First")
    assertEquals(added.read, "First")

    val removed = added.write("")
    assertEquals(removed.read, "")
  }

  test("Dotted DeltaBuffer can contain non-contextual LastWriterWins[String]") {
    val lww: DeltaBuffer[Dotted[LastWriterWins[String]]] = DeltaBuffer(Dotted.empty)

    assertEquals(lww.data.read, "")

    val added = lww.write("First")
    assertEquals(added.data.read, "First")

    val removed = added.write("")
    assertEquals(removed.data.read, "")
  }

  test("Dotted DeltaBufferContainer can contain non-contextual LastWriterWins[String]") {
    val lww: DeltaBufferContainer[Dotted[LastWriterWins[String]]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assertEquals(lww.data.read, "")

    lww.write("First")
    assertEquals(lww.data.read, "First")

    lww.write("")
    assertEquals(lww.data.read, "")
  }

  // END LastWriterWins

  // START AuctionData

  test("plain AuctionData without container returns deltas") {
    val auction: AuctionData = AuctionData.empty

    assertEquals(auction.bids, Set.empty)
    assertEquals(auction.status, AuctionInterface.Open)
    assertEquals(auction.winner, None)

    val added_delta = auction.bid("First", 1)
    assertEquals(added_delta.bids, Set(Bid("First", 1)))
    assertEquals(added_delta.status, AuctionInterface.Open)
    assertEquals(added_delta.winner, None)

    val added: AuctionData = auction merge added_delta

    val knockedDown_delta: AuctionData = added.knockDown()
    assertEquals(knockedDown_delta.bids, Set.empty)
    assertEquals(knockedDown_delta.status, AuctionInterface.Closed)
    assertEquals(knockedDown_delta.winner, None)

    val knockedDown: AuctionData = added merge knockedDown_delta

    assertEquals(knockedDown.bids, Set(Bid("First", 1)))
    assertEquals(knockedDown.status, AuctionInterface.Closed)
    assertEquals(knockedDown.winner, Some("First"))
  }

  test("Dotted can contain plain AuctionData") {
    val auction: Dotted[AuctionData] = Dotted.empty

    assertEquals(auction.data.bids, Set.empty)
    assertEquals(auction.data.status, AuctionInterface.Open)
    assertEquals(auction.data.winner, None)

    val added = auction.modn(_.bid("First", 1))
    assertEquals(added.data.bids, Set(Bid("First", 1)))
    assertEquals(added.data.status, AuctionInterface.Open)
    assertEquals(added.data.winner, None)

    val knockedDown = added merge added.modn(_.knockDown())
    assertEquals(knockedDown.data.bids, Set(Bid("First", 1)))
    assertEquals(knockedDown.data.status, AuctionInterface.Closed)
    assertEquals(knockedDown.data.winner, Some("First"))
  }

  test("Dotted DeltaBuffer can contain plain AuctionData") {
    val auction: DeltaBuffer[AuctionData] = DeltaBuffer(AuctionData.empty)

    assertEquals(auction.state.bids, Set.empty)
    assertEquals(auction.state.status, AuctionInterface.Open)
    assertEquals(auction.state.winner, None)

    val added = auction.mod(_.bid("First", 1))
    assertEquals(added.state.bids, Set(Bid("First", 1)))
    assertEquals(added.state.status, AuctionInterface.Open)
    assertEquals(added.state.winner, None)

    val knockedDown = added.mod(_.knockDown())
    assertEquals(knockedDown.state.bids, Set(Bid("First", 1)))
    assertEquals(knockedDown.state.status, AuctionInterface.Closed)
    assertEquals(knockedDown.state.winner, Some("First"))
  }


  test("Dotted DeltaBufferContainer can contain plain AuctionData") {
    val auction: DeltaBufferContainer[Dotted[AuctionData]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assertEquals(auction.result.state.data.bids, Set.empty)
    assertEquals(auction.result.state.data.status, AuctionInterface.Open)
    assertEquals(auction.result.state.data.winner, None)

    auction.modn(_.bid("First", 1))
    assertEquals(auction.result.state.data.bids, Set(Bid("First", 1)))
    assertEquals(auction.result.state.data.status, AuctionInterface.Open)
    assertEquals(auction.result.state.data.winner, None)

    auction.modn(_.knockDown())
    assertEquals(auction.result.state.data.bids, Set(Bid("First", 1)))
    assertEquals(auction.result.state.data.status, AuctionInterface.Closed)
    assertEquals(auction.result.state.data.winner, Some("First"))
  }

  // END AuctionData

}
