package benchmarks.encrdt

import benchmarks.encrdt.Codecs.awlwwmapJsonCodec
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import com.google.crypto.tink.Aead
import encrdtlib.container.AddWinsLastWriterWinsMap
import kofre.time.VectorClock
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import encrdtlib.encrypted.statebased.DecryptedState.vectorClockJsonCodec

import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
@Warmup(iterations = 4, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(5)
@Threads(1)
class AWLWWMapBenchmark {
  val replicaId = "TestReplica"

  @Benchmark
  def serializeOnly(blackhole: Blackhole, serializeOnlyBenchmarkState: SerializeOnlyBenchmarkState): Unit = {
    val serialPlaintextState = writeToArray(serializeOnlyBenchmarkState.crdtState)
    blackhole.consume(serialPlaintextState)
  }

  @Benchmark
  @Warmup(iterations = 7)
  def encryptOnly(
      blackhole: Blackhole,
      serializeOnlyBenchmarkState: SerializeOnlyBenchmarkState,
      aeadState: AeadState
  ): Unit = {
    val serialEncryptedState = aeadState.aead.encrypt(
      serializeOnlyBenchmarkState.serialPlaintextState,
      serializeOnlyBenchmarkState.serialPlaintextVectorClock
    )
    blackhole.consume((serialEncryptedState, serializeOnlyBenchmarkState.serialPlaintextState))
  }

  @Benchmark
  @Warmup(iterations = 10)
  def serializeAndEncrypt(
      blackhole: Blackhole,
      serializeOnlyBenchmarkState: SerializeOnlyBenchmarkState,
      aeadState: AeadState
  ): Unit = {
    val serialPlaintextState       = writeToArray(serializeOnlyBenchmarkState.crdtState)
    val serialPlaintextVectorClock = writeToArray(serializeOnlyBenchmarkState.crdtStateVersionVector)
    val serialEncryptedState       = aeadState.aead.encrypt(serialPlaintextState, serialPlaintextVectorClock)
    blackhole.consume((serialEncryptedState, serialPlaintextState))
  }

  @Benchmark
  def putOnceNoSerialization(benchState: SerializeOnlyBenchmarkState): Unit = {
    benchState.crdt.put("This is a String", "And so this is too")
  }

  @Benchmark
  @Fork(2)
  @Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
  def putAndSerializeManyTimes(blackhole: Blackhole, putBenchmarkState: PutManyBenchmarkState): Unit = {
    val crdt = new AddWinsLastWriterWinsMap[String, String](replicaId)
    for (entry <- putBenchmarkState.dummyKeyValuePairs) {
      // Update crdt
      crdt.put(entry._1, entry._2)
      // Serialize to JSON (as byte array)
      val serializedState = writeToArray(crdt.state)

      blackhole.consume(serializedState)
    }
  }

  @Benchmark
  @Fork(2)
  @Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
  def putAndSerializeAndEncryptManyTimes(
      blackhole: Blackhole,
      putBenchmarkState: PutManyBenchmarkState,
      aeadState: AeadState
  ): Unit = {
    var versionVector: VectorClock = VectorClock.zero
    val crdt                       = new AddWinsLastWriterWinsMap[String, String](replicaId)
    val aead                       = aeadState.aead

    for (entry <- putBenchmarkState.dummyKeyValuePairs) {
      // Update crdt
      crdt.put(entry._1, entry._2)
      // Track time information used for encrypted crdt
      versionVector = versionVector.inc(replicaId)
      // Serialize/Encrypt/Authenticate state with attached time
      val serialState       = writeToArray(crdt.state)
      val serialVectorClock = writeToArray(versionVector)
      val encryptedState    = aead.encrypt(serialState, serialVectorClock)

      blackhole.consume((encryptedState, serialState))
    }
  }
}

@State(Scope.Thread)
class AeadState {
  @Param(Array("AES128_GCM", "AES256_GCM", "AES256_GCM_SIV", "XCHACHA20_POLY1305"))
  var keyTemplateString: String = scala.compiletime.uninitialized

  var aead: Aead = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setupAead(): Unit = {
    aead = Helper.setupAead(keyTemplateString)
  }
}

@State(Scope.Thread)
class SerializeOnlyBenchmarkState {
  var crdt: AddWinsLastWriterWinsMap[String, String]                  = scala.compiletime.uninitialized
  var crdtState: AddWinsLastWriterWinsMap.LatticeType[String, String] = scala.compiletime.uninitialized
  var crdtStateVersionVector: VectorClock                             = scala.compiletime.uninitialized

  var serialPlaintextState: Array[Byte]       = scala.compiletime.uninitialized
  var serialPlaintextVectorClock: Array[Byte] = scala.compiletime.uninitialized

  @Param(Array("10", "100", "1000"))
  var crdtSizeInElements: Int = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setupCrdtState(): Unit = {
    val dummyKeyValuePairs = Helper.dummyKeyValuePairs(crdtSizeInElements)

    var versionVector: VectorClock = VectorClock.zero
    val replicaId                  = "TestReplica"
    val crdt                       = new AddWinsLastWriterWinsMap[String, String](replicaId)

    for (entry <- dummyKeyValuePairs) {
      // Update crdt
      crdt.put(entry._1, entry._2)
      // Track time information used for encrypted crdt
      versionVector = versionVector.inc(replicaId)
    }

    this.crdt = crdt
    this.crdtState = crdt.state
    this.crdtStateVersionVector = versionVector

    this.serialPlaintextState = writeToArray(crdtState)
    this.serialPlaintextVectorClock = writeToArray(crdtStateVersionVector)
  }
}

@State(Scope.Thread)
class PutManyBenchmarkState {
  var dummyKeyValuePairs: Array[(String, String)] = scala.compiletime.uninitialized

  @Param(Array("10", "100", "1000"))
  var crdtSizeInElements: Int = scala.compiletime.uninitialized

  @Setup(Level.Trial)
  def setupTestData(): Unit = {
    dummyKeyValuePairs = Helper.dummyKeyValuePairs(crdtSizeInElements)
  }
}
