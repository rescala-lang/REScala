import rdts.datatypes.experiments.protocols.paper.Paxos
class PaperPaxosSuite extends munit.ScalaCheckSuite:

  //  override def scalaCheckInitialSeed = "ZcBq5Oa3t8-hWG0Snkx22h6nivxFRCvp27NO4tFKzbN="

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(200)
      .withMinSize(60)
      .withMaxSize(200)

  property("Paper Paxos")(ConsensusPropertySpec[Int, Paxos](
    logging = true,
    minDevices = 3,
    maxDevices = 7,
    writeFreq = 20,
    mergeFreq = 70
  ).property())
