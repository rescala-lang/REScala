import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Arbitrary, Gen, Prop}
import rdts.base.LocalUid
import rdts.datatypes.experiments.protocols.simplified

import scala.util.Try

class SimplePaxosSuite extends munit.ScalaCheckSuite {

  //  override def scalaCheckInitialSeed = "yL7jhVAhl4I5iCmRP_WmL07-3jaoICgGS7X0-zv54LD="

  //  override def scalaCheckTestParameters =
  //    super.scalaCheckTestParameters
  //      .withMinSuccessfulTests(500)
  //      .withMinSize(30)
  //      .withMaxSize(200)

  property("Paxos simplified")(SimplePaxosSpec[Int](
    logging = false,
    minDevices = 3,
    maxDevices = 3,
    writeFreq = 20,
    upkeepFreq = 70,
    mergeFreq = 70
  ).property())
}

class SimplePaxosSpec[A: Arbitrary](
    logging: Boolean = false,
    minDevices: Int,
    maxDevices: Int,
    writeFreq: Int,
    upkeepFreq: Int,
    mergeFreq: Int
) extends ConsensusPropertySpec[A, simplified.Paxos](
      logging,
      minDevices,
      maxDevices,
      writeFreq,
      upkeepFreq,
      mergeFreq
    ) {

  override def genUpkeep(state: Map[LocalUid, simplified.Paxos[A]]): Gen[Upkeep] =
    for
      id <- genId(state)
    yield PUpkeep(id)

  class PUpkeep(id: LocalUid) extends Upkeep(id) {
    override def postCondition(
        state: Map[LocalUid, simplified.Paxos[A]],
        result: Try[Map[LocalUid, simplified.Paxos[A]]]
    ): Prop =
      val res            = result.get
      val doubleProposal = res(id).accepts.groupBy(_.proposal).find(_._2.size > 1)
      doubleProposal.isEmpty :| s"Only one proposal for a given proposal number. Found: $doubleProposal" &&
      super.postCondition(state, result)
  }
}
