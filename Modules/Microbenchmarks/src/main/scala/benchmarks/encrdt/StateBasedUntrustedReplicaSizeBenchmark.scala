package benchmarks.encrdt

import benchmarks.encrdt.Codecs.deltaAwlwwmapJsonCodec
import benchmarks.encrdt.statebased.{DecryptedState, EncryptedState, UntrustedReplica}
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import rdts.syntax.DeltaAWLWWMContainer
import rdts.syntax.DeltaAWLWWMContainer.State
import rdts.time.VectorClock

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

object StateBasedUntrustedReplicaSizeBenchmark extends App {
  val minElementExponent   = 1 // 10 ** this as minimum tested total elements added to CRDT
  val maxElementExponent   = 3 // 10 ** this as maximum tested total elements added to CRDT
  val MAX_TESTED_ELEMENTS  = Math.pow(10, maxElementExponent.toDouble).toInt
  val MAX_PARALLEL_UPDATES = 4

  val outDir = Paths.get("./benchmarks/results/")
  if !outDir.toFile.exists() then
    outDir.toFile.mkdirs()
    ()
  val csvFile   = new PrintWriter(Files.newOutputStream(outDir.resolve("state_size_benchmark.csv")))
  val csvHeader = "concurrentUpdates,commonElements,uniqueElements,untrustedReplicaSize,mergedSize"
  println(csvHeader)
  csvFile.println(csvHeader)
  val dummyKeyValuePairs = Helper.dummyKeyValuePairs(MAX_TESTED_ELEMENTS)
  val aead               = AeadTranslation(Helper.setupAead("AES128_GCM"))

  for totalElements <- (minElementExponent to maxElementExponent).map(i => math.pow(10, i.toDouble).toInt) do {
    val crdt                       = new DeltaAWLWWMContainer[String, String]("0")
    var versionVector: VectorClock = VectorClock.zero

    for i <- 0 until totalElements - MAX_PARALLEL_UPDATES do {
      val entry = dummyKeyValuePairs(i)
      crdt.put(entry._1, entry._2)
      versionVector = versionVector.inc("0")
    }

    for parallelUpdates <- 1 to MAX_PARALLEL_UPDATES do {
      for i <- (totalElements - MAX_PARALLEL_UPDATES) to (totalElements - parallelUpdates) do {
        val entry = dummyKeyValuePairs(i)
        crdt.put(entry._1, entry._2)
        versionVector = versionVector.inc("0")
      }

      val commonState      = crdt.state
      val commonStateDec   = DecryptedState(commonState, versionVector)
      val commonStateEnc   = commonStateDec.encrypt(AeadTranslation(Helper.setupAead("AES128_GCM")))
      val untrustedReplica = new UntrustedStateBasedReplicaMock(Set(commonStateEnc))

      var decryptedStatesMerged: DecryptedState[State[String, String]] = commonStateDec

      for replicaId <- 1 to parallelUpdates do {
        val entry               = dummyKeyValuePairs(totalElements - parallelUpdates + replicaId - 1)
        val replicaSpecificCrdt = new DeltaAWLWWMContainer[String, String](replicaId.toString, commonState)
        replicaSpecificCrdt.put(entry._1, entry._2)
        val replicaSpecificVersionVector = versionVector.inc(replicaId.toString)
        val replicaSpecificDecState: DecryptedState[State[String, String]] =
          DecryptedState(replicaSpecificCrdt.state, replicaSpecificVersionVector)
        val replicaSpecificEncState = replicaSpecificDecState.encrypt(aead)
        untrustedReplica.receive(replicaSpecificEncState)
        decryptedStatesMerged =
          DecryptedState.lattice[State[String, String]](DeltaAWLWWMContainer.lattice).merge(
            decryptedStatesMerged,
            replicaSpecificDecState
          )
      }

      val mergedSize = writeToArray(decryptedStatesMerged.state).length
      val csvLine =
        s"$parallelUpdates,${totalElements - parallelUpdates},$totalElements,${untrustedReplica.size},$mergedSize"
      println(csvLine)
      csvFile.println(csvLine)
    }
  }

  csvFile.close()
}

class UntrustedStateBasedReplicaMock(encryptedStates: Set[EncryptedState]) extends UntrustedReplica(encryptedStates) {
  override protected def disseminate(encryptedState: EncryptedState): Unit = {}

  def size: Int = {
    stateStore.toList.map { encState =>
      encState.serialVersionVector.length + encState.stateCiphertext.length
    }.sum
  }
}
