package test.rdts.containers

import rdts.base.Uid
import rdts.datatypes.experiments.AuctionInterface.AuctionData
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, DeltaBufferContainer, LocalUid}

class DeltaBufferContainerDottedTest extends munit.FunSuite {

  test("nested context removal") {
    given LocalUid = Uid.gen()

    val initial = DeltaBufferContainer[Dotted[AuctionData]](DeltaBuffer(Dotted(AuctionData())))

    assertEquals(initial.result.state.data, AuctionData())
    assert(initial.result.state.data.bids.isEmpty)
    assertEquals(initial.result.deltaBuffer, List.empty)

    initial.bid(userId = "Testuser", price = 7)

    assertEquals(initial.result.state.data.bids.size, 1)
    assertEquals(initial.result.deltaBuffer.size, 1)
  }

}
