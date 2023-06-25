package test.kofre

import kofre.base.Bottom
import kofre.base.Lattice
import kofre.base.Lattice.Operators
import kofre.time.{Dots, Dot}
import kofre.dotted.HasDots.*
import kofre.datatypes.contextual.AddWinsSet
import kofre.datatypes.contextual.EnableWinsFlag
import kofre.datatypes.contextual.LastWriterWins
import kofre.datatypes.experiments.AuctionInterface
import kofre.datatypes.experiments.AuctionInterface.AuctionData
import kofre.datatypes.experiments.AuctionInterface.Bid
import kofre.dotted.{Dotted, HasDots}
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.*
import kofre.syntax.DeltaBuffer
import kofre.syntax.DeltaBufferContainer
import kofre.syntax.ReplicaId
import kofre.base.Uid.asId

class ContainerTest extends munit.FunSuite {

  object helper {

    given r: ReplicaId = "me".asId

    // it would be nicer to ask the context, but for that we need the container, for that we need an empty LWW...
    val initialDot: Dot = Dot(r.uid, 0)

    given bottomString: Bottom[String] with {
      override def empty: String = ""
    }

    given bottomLLW[A: Bottom]: Bottom[LastWriterWins[A]] with {
      override def empty: LastWriterWins[A] = LastWriterWins.empty(initialDot)
    }

  }

  import helper.given

  // START EnableWinsFlag

  test("Dotted can contain contextual EnableWinsFlag") {
    val flag: Dotted[EnableWinsFlag] = Dotted.empty
    given ReplicaId = "me".asId

    assertEquals(flag.read, false)

    val enabled = flag.enable()
    assertEquals(enabled.read, true)

    val disabled = enabled.disable()
    assertEquals(disabled.read, false)
  }

  // NOTE: DeltaBuffer cannot contain contextual EnableWinsFlag without Dotted, as EnableWinsFlag needs a context

  test("Dotted DeltaBuffer can contain contextual EnableWinsFlag") {
    val flag: DeltaBuffer[Dotted[EnableWinsFlag]] = DeltaBuffer(Dotted.empty)

    assertEquals(flag.read, false)

    val enabled = flag.enable()
    assertEquals(enabled.read, true)

    val disabled = enabled.disable()
    assertEquals(disabled.read, false)
  }

  test("Dotted DeltaBufferContainer can contain contextual EnableWinsFlag") {
    val flag: DeltaBufferContainer[Dotted[EnableWinsFlag]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assertEquals(flag.read, false)

    flag.enable()
    assertEquals(flag.read, true)

    flag.disable()
    assertEquals(flag.read, false)
  }

  // END EnableWinsFlag


  // START AddWinsSet

  test("Dotted can contain contextual AddWinsSet[String]") {
    val awSet: Dotted[AddWinsSet[String]] = Dotted.empty

    assert(awSet.elements.isEmpty)

    val added = awSet.add("First")
    assertEquals(added.elements.size, 1)
    assert(added.elements.contains("First"))
    assert(added.contains("First"))

    val removed = added.remove("First")
    assert(removed.elements.isEmpty)
  }

  // NOTE: DeltaBuffer cannot contain contextual AddWinsSet without Dotted, as AddWinsSet needs a context

  test("Dotted DeltaBuffer can contain contextual AddWinsSet[String]") {
    val awSet: DeltaBuffer[Dotted[AddWinsSet[String]]] = DeltaBuffer(Dotted.empty)

    assert(awSet.elements.isEmpty)

    val added = awSet.add("First")
    assertEquals(added.elements.size, 1)
    assert(added.elements.contains("First"))
    assert(added.contains("First"))

    val removed = added.remove("First")
    assert(removed.elements.isEmpty)
  }

  test("Dotted DeltaBufferContainer can contain contextual AddWinsSet[String]") {
    val awSet: DeltaBufferContainer[Dotted[AddWinsSet[String]]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assert(awSet.elements.isEmpty)

    awSet.add("First")
    assertEquals(awSet.elements.size, 1)
    assert(awSet.elements.contains("First"))
    assert(awSet.contains("First"))

    awSet.remove("First")
    assert(awSet.elements.isEmpty)
  }

  // END AddWinsSet


  // START LastWriterWins

  test("Dotted can contain contextual LastWriterWins[String]") {
    val lww: Dotted[LastWriterWins[String]] = Dotted.empty

    assertEquals(lww.read, "")

    val added = lww.write("First")
    assertEquals(added.read, "First")

    val removed = added.write("")
    assertEquals(removed.read, "")
  }

  // NOTE: DeltaBuffer cannot contain contextual LastWriterWins without Dotted, as LastWriterWins needs a context

  test("Dotted DeltaBuffer can contain contextual LastWriterWins[String]") {
    val lww: DeltaBuffer[Dotted[LastWriterWins[String]]] = DeltaBuffer(Dotted.empty)

    assertEquals(lww.read, "")

    val added = lww.write("First")
    assertEquals(added.read, "First")

    val removed = added.write("")
    assertEquals(removed.read, "")
  }

  test("Dotted DeltaBufferContainer can contain contextual LastWriterWins[String]") {
    val lww: DeltaBufferContainer[Dotted[LastWriterWins[String]]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assertEquals(lww.read, "")

    lww.write("First")
    assertEquals(lww.read, "First")

    lww.write("")
    assertEquals(lww.read, "")
  }

  // END LastWriterWins


  // START AuctionData

  test("Dotted can contain AuctionData") {
    val auction: Dotted[AuctionData] = Dotted.empty

    assertEquals(auction.data.bids,   Set.empty)
    assertEquals(auction.data.status, AuctionInterface.Open)
    assertEquals(auction.data.winner, None)

    val added = auction.bid("First", 1)
    assertEquals(added.data.bids,   Set(Bid("First", 1)))
    assertEquals(added.data.status, AuctionInterface.Open)
    assertEquals(added.data.winner, None)

    val closed = added.close()
    assertEquals(closed.data.bids,   Set(Bid("First", 1)))
    assertEquals(closed.data.status, AuctionInterface.Closed)
    assertEquals(closed.data.winner, Some("First"))
  }

  test("Dotted DeltaBuffer can contain plain AuctionData") {
    val auction: DeltaBuffer[AuctionData] = DeltaBuffer(AuctionData.empty)

    assertEquals(auction.state.bids,   Set.empty)
    assertEquals(auction.state.status, AuctionInterface.Open)
    assertEquals(auction.state.winner, None)

    val added = auction.bid("First", 1)
    assertEquals(added.state.bids,   Set(Bid("First", 1)))
    assertEquals(added.state.status, AuctionInterface.Open)
    assertEquals(added.state.winner, None)

    val closed = added.close()
    assertEquals(closed.state.bids,   Set(Bid("First", 1)))
    assertEquals(closed.state.status, AuctionInterface.Closed)
    assertEquals(closed.state.winner, Some("First"))
  }

  test("Dotted DeltaBuffer can contain plain AuctionData") {
    val auction: DeltaBuffer[Dotted[AuctionData]] = DeltaBuffer(Dotted.empty)

    assertEquals(auction.state.data.bids,   Set.empty)
    assertEquals(auction.state.data.status, AuctionInterface.Open)
    assertEquals(auction.state.data.winner, None)

    val added = auction.bid("First", 1)
    assertEquals(added.state.data.bids,   Set(Bid("First", 1)))
    assertEquals(added.state.data.status, AuctionInterface.Open)
    assertEquals(added.state.data.winner, None)

    val closed = added.close()
    assertEquals(closed.state.data.bids,   Set(Bid("First", 1)))
    assertEquals(closed.state.data.status, AuctionInterface.Closed)
    assertEquals(closed.state.data.winner, Some("First"))
  }

  test("Dotted DeltaBufferContainer can contain plain AuctionData") {
    val auction: DeltaBufferContainer[Dotted[AuctionData]] = DeltaBufferContainer(DeltaBuffer(Dotted.empty))

    assertEquals(auction.result.state.data.bids,   Set.empty)
    assertEquals(auction.result.state.data.status, AuctionInterface.Open)
    assertEquals(auction.result.state.data.winner, None)

    auction.bid("First", 1)
    assertEquals(auction.result.state.data.bids,   Set(Bid("First", 1)))
    assertEquals(auction.result.state.data.status, AuctionInterface.Open)
    assertEquals(auction.result.state.data.winner, None)

    auction.close()
    assertEquals(auction.result.state.data.bids,   Set(Bid("First", 1)))
    assertEquals(auction.result.state.data.status, AuctionInterface.Closed)
    assertEquals(auction.result.state.data.winner, Some("First"))
  }

  // END AuctionData

}
