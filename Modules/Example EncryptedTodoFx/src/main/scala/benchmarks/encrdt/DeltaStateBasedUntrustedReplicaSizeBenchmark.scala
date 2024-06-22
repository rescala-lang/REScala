package benchmarks.encrdt

import benchmarks.encrdt.Codecs.given
import benchmarks.encrdt.mock.UntrustedDeltaBasedReplicaMock
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import com.google.crypto.tink.Aead
import encrdtlib.container.DeltaAWLWWMContainer
import encrdtlib.encrypted.deltabased.DecryptedDeltaGroup
import rdts.base.Bottom
import rdts.time.{Dot, Dots}

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}

object DeltaStateBasedUntrustedReplicaSizeBenchmark extends App with DeltaStateUntrustedReplicaSizeBenchEnvironment {
  val csvFile = new PrintWriter(Files.newOutputStream(Paths.get("./benchmarks/results/delta_state_size_benchmark.csv")))
  println(csvHeader)
  csvFile.println(csvHeader)

  val minElementExponent = 1 // 10 ** this as minimum tested total elements added to CRDT
  val maxElementExponent = 3 // 10 ** this as maximum tested total elements added to CRDT
  for totalElements <- (minElementExponent to maxElementExponent).map(i => math.pow(10d, i.toDouble).toInt) do {
    // val totalElements     = 10_000 // Commment out, if using the loop above
    val maxParallelStates = 4
    val elementsInCommon  = totalElements - maxParallelStates

    val benchmarkSharedUntrustedReplica = new UntrustedDeltaBasedReplicaMock;
    var benchmarkSharedCurrentDot       = Dot("0", 0);
    val benchmarkSharedCrdt: DeltaAWLWWMContainer[String, String] =
      new DeltaAWLWWMContainer[String, String]("0")

    for i <- 0 until elementsInCommon do {
      val entry = dummyKeyValuePairs(i)
      benchmarkSharedCurrentDot = benchmarkSharedCurrentDot.advance
      val delta    = benchmarkSharedCrdt.putDelta(entry._1, entry._2)
      val encDelta = DecryptedDeltaGroup(delta, Dots.single(benchmarkSharedCurrentDot)).encrypt(aead)
      benchmarkSharedUntrustedReplica.receive(encDelta)
    }

    for parallelStates <- 1 to maxParallelStates do {
      val crdt =
        new DeltaAWLWWMContainer[String, String]("0", benchmarkSharedCrdt.state)
      val untrustedReplica = benchmarkSharedUntrustedReplica.copy()
      var localDot         = Dot("0", 0);
      // Populate CRDT with missing elements (before adding concurrent updates)
      {
        for i <- elementsInCommon until (totalElements - parallelStates) do {
          val entry = dummyKeyValuePairs(i)
          localDot = localDot.advance
          val delta    = crdt.putDelta(entry._1, entry._2)
          val encDelta = DecryptedDeltaGroup(delta, Dots.single(localDot)).encrypt(aead)
          untrustedReplica.receive(encDelta)
        }

        var unmergedDeltas = List.empty[DeltaAWLWWMContainer.StateType[String, String]]
        for replicaId <- 1 to parallelStates do {
          val entry = dummyKeyValuePairs(totalElements - replicaId)
          val replicaSpecificCrdt =
            new DeltaAWLWWMContainer[String, String](replicaId.toString, crdt.state)
          val delta = replicaSpecificCrdt.putDelta(entry._1, entry._2)
          unmergedDeltas = unmergedDeltas :+ delta
          val dot      = Dot(replicaId.toString, 1)
          val encState = DecryptedDeltaGroup(delta, Dots.single(dot)).encrypt(aead)
          untrustedReplica.receive(encState)
        }

        val mergedCrdt = new DeltaAWLWWMContainer[String, String]("0", crdt.state)
        unmergedDeltas.foreach(delta => mergedCrdt.merge(delta))
        val serializedDecryptedMergedState = writeToArray(mergedCrdt.state)

        val mergedSize      = serializedDecryptedMergedState.length
        val mergedEncrypted = DecryptedDeltaGroup(mergedCrdt.state, untrustedReplica.getCausalContext).encrypt(aead)
        val mergedEncryptedSize =
          mergedEncrypted.serialDottedVersionVector.length + mergedEncrypted.stateCiphertext.length
        val csvLine =
          s"$parallelStates,${totalElements - parallelStates},$totalElements,${untrustedReplica.size()},$mergedSize,$mergedEncryptedSize"
        println(csvLine)
        csvFile.println(csvLine)
      }
    }
  }

  csvFile.close()
}

object DeltaStateBasedUntrustedReplicaSizeBenchmarkLinearScaling extends App
    with DeltaStateUntrustedReplicaSizeBenchEnvironment {
  val csvFile = new PrintWriter(
    Files.newOutputStream(Paths.get("./benchmarks/results/delta_state_size_benchmark_linear_sampling.csv"))
  )
  println(csvHeader)
  csvFile.println(csvHeader)
  val crdt: DeltaAWLWWMContainer[String, String] = new DeltaAWLWWMContainer[String, String]("0")
  var currentDot                                 = Dot("0", 0)

  val untrustedReplica = new UntrustedDeltaBasedReplicaMock()

  val totalElements = 10_000
  for i <- 0 until totalElements do {
    val entry = dummyKeyValuePairs(i)
    val delta = crdt.putDelta(entry._1, entry._2)
    currentDot = currentDot.advance
    val encDelta = DecryptedDeltaGroup(delta, Dots.single(currentDot)).encrypt(aead)
    untrustedReplica.receive(encDelta)

    if (i + 1) % 1_000 == 0 then {
      val serializedInternalCrdt = writeToArray(crdt.state)
      val encrdtFullyMerged      = DecryptedDeltaGroup(crdt.state, untrustedReplica.getCausalContext).encrypt(aead)
      val encrdtFullyMergedSize =
        encrdtFullyMerged.stateCiphertext.length + encrdtFullyMerged.serialDottedVersionVector.length

      val csvLine =
        s"1,${i + 1},${i + 1},${untrustedReplica.size()},${serializedInternalCrdt.length},$encrdtFullyMergedSize"
      println(csvLine)
      csvFile.println(csvLine)
    }
  }

  csvFile.close()
}

trait DeltaStateUntrustedReplicaSizeBenchEnvironment {
  val csvHeader = "concurrentUpdates,commonElements,uniqueElements,untrustedReplicaSize,mergedSize,mergedEncryptedSize"

  val outDir: Path = Paths.get("./", "benchmarks", "results")
  if !outDir.toFile.exists() then
    outDir.toFile.mkdirs()
    ()
  val aead: replication.Aead                                  = AeadTranslation(Helper.setupAead("AES128_GCM"))
  val dummyKeyValuePairs: Array[(String, String)] = Helper.dummyKeyValuePairs(10_000)
}

class AeadTranslation(aead: com.google.crypto.tink.Aead) extends replication.Aead {
  override def encrypt(data: Array[Byte], associated: Array[Byte]): Array[Byte] = aead.encrypt(data, associated)

  override def decrypt(data: Array[Byte], associated: Array[Byte]): Array[Byte] = aead.decrypt(data, associated)
}
