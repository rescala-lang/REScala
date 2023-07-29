package test.kofre.containers

import kofre.base.Uid
import kofre.datatypes.experiments.AuctionInterface.AuctionData
import kofre.dotted.Dotted
import kofre.syntax.{DeltaBuffer, DeltaBufferContainer, ReplicaId}

class DeltaBufferContainerDottedTest extends munit.FunSuite {

  test("nested context removal") {
    given ReplicaId = Uid.gen()

    val initial = DeltaBufferContainer[Dotted[AuctionData]](DeltaBuffer(Dotted(AuctionData())))

    assertEquals(initial.result.state.data, AuctionData())
    assert(initial.result.state.data.bids.isEmpty)
    assertEquals(initial.result.deltaBuffer, List.empty)

    initial.bid(userId = "Testuser", price = 7)

    assertEquals(initial.result.state.data.bids.size, 1)
    assertEquals(initial.result.deltaBuffer.size, 1)
  }

}
