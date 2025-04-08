package test.rdts

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Arbitrary, Gen, Prop}
import rdts.base.LocalUid
import rdts.datatypes.experiments.protocols.old.simplified.Paxos

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
    mergeFreq = 70
  ).property())
}

class SimplePaxosSpec[A: Arbitrary](
    logging: Boolean = false,
    minDevices: Int,
    maxDevices: Int,
    writeFreq: Int,
    mergeFreq: Int
) extends ConsensusPropertySpec[A, Paxos](
      logging,
      minDevices,
      maxDevices,
      writeFreq,
      mergeFreq
    ) {
  override def genWrite(state: State): Gen[Write] =
    for
      id    <- genId(state)
      value <- arbitrary[A]
    yield PWrite(id, value)

  class PWrite(id: LocalUid, value: A) extends Write(id, value) {
    override def postCondition(
        state: Map[LocalUid, Paxos[A]],
        result: Try[Map[LocalUid, Paxos[A]]]
    ): Prop =
      val res            = result.get
      val doubleProposal = res(id).accepts.groupBy(_.proposal).find(_._2.size > 1)
      doubleProposal.isEmpty :| s"Only one proposal for a given proposal number. Found: $doubleProposal" &&
      super.postCondition(state, result)
  }
}
