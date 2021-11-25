package de.ckuessner
package encrdt.benchmarks

import encrdt.benchmarks.Codecs.awlwwmapJsonCodec
import encrdt.causality.VectorClock
import encrdt.crdts.AddWinsLastWriterWinsMap
import encrdt.encrypted.statebased.DecryptedState.vectorClockJsonCodec

import com.github.javafaker.Faker
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import com.google.crypto.tink.aead.AeadConfig
import com.google.crypto.tink.{Aead, KeyTemplates, KeysetHandle}
import org.conscrypt.Conscrypt
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.security.Security
import java.util.Random
import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
@Warmup(iterations = 10, time = 2000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 2000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(5)
@Threads(1)
class AWLWWMapBenchmark {
  var entries: Array[(String, String)] = _
  var aead: Aead = _

  val replicaId = "TestReplica"

  @Param(Array("1000"))
  var putOperations: Int = _

  @Setup(Level.Trial)
  def setupTestData(): Unit = {
    val faker = new Faker(new Random(42))
    entries = new Array[(String, String)](putOperations)
    for (i <- entries.indices) {
      entries(i) = faker.name().fullName() -> faker.address().fullAddress()
    }
  }

  @Benchmark
  def putAndSerializePlaintext(blackhole: Blackhole): Unit = {
    val crdt = new AddWinsLastWriterWinsMap[String, String](replicaId)
    for (entry <- entries) {
      // Update crdt
      crdt.put(entry._1, entry._2)
      // Serialize to JSON (as byte array)
      val serializedState = writeToArray(crdt.state)

      blackhole.consume(serializedState)
    }
  }

  @Benchmark
  def putAndSerializeAndEncrypt(blackhole: Blackhole, aeadState: AeadState): Unit = {
    var versionVector: VectorClock = VectorClock()
    val crdt = new AddWinsLastWriterWinsMap[String, String](replicaId)
    val aead = aeadState.aead

    for (entry <- entries) {
      // Update crdt
      crdt.put(entry._1, entry._2)
      // Track causality information used for encrypted crdt
      versionVector = versionVector.advance(replicaId)
      // Serialize/Encrypt/Authenticate state with attached causality
      val serialState = writeToArray(crdt.state)
      val serialVectorClock = writeToArray(versionVector)
      val encryptedState = aead.encrypt(serialState, serialVectorClock)

      blackhole.consume(encryptedState, serialState)
    }
  }

}

@State(Scope.Thread)
class AeadState {
  @Param(Array("AES128_GCM", "AES256_GCM", "AES256_GCM_SIV", "XCHACHA20_POLY1305"))
  var keyTemplateString: String = _

  var aead: Aead = _

  @Setup(Level.Trial)
  def setupAead(): Unit = {
    Conscrypt.checkAvailability()
    Security.addProvider(Conscrypt.newProvider)
    AeadConfig.register()
    val keyset: KeysetHandle = KeysetHandle.generateNew(KeyTemplates.get(keyTemplateString))
    aead = keyset.getPrimitive(classOf[Aead])
  }
}

