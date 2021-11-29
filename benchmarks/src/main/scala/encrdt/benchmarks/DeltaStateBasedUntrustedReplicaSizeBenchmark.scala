package de.ckuessner
package encrdt.benchmarks

import encrdt.benchmarks.Codecs.deltaAwlwwmapJsonCodec
import encrdt.causality.LamportClock
import encrdt.crdts.DeltaAddWinsLastWriterWinsMap
import encrdt.encrypted.deltabased.{DecryptedDeltaGroup, EncryptedDeltaGroup, UntrustedReplica}

import com.github.plokhotnyuk.jsoniter_scala.core.{writeToArray, writeToString}
import com.google.crypto.tink.Aead

import java.io.{ByteArrayOutputStream, ObjectOutputStream, PrintWriter}
import java.nio.file.{Files, Paths}

object DeltaStateBasedUntrustedReplicaSizeBenchmark extends App {
  val debug = false
  val outDir = Paths.get("./", "benchmarks", "out")
  if (!outDir.toFile.exists()) outDir.toFile.mkdirs()
  val debugOutDir = outDir.resolve("debug")
  if (debug) {
    val dirFile = debugOutDir.toFile
    if (!dirFile.exists()) dirFile.mkdirs()
  }

  val csvFile = new PrintWriter(Files.newOutputStream(Paths.get("./benchmarks/out/delta_state_size_benchmark.csv")))
  val header = "concurrentUpdates,commonElements,uniqueElements,untrustedReplicaSize,mergedSize"
  println(header)
  csvFile.println(header)

  val aead = Helper.setupAead("AES128_GCM")

  for (parallelStates <- 1 to 4) {
    for (commonElements <- (1 to 4).map(i => math.pow(10, i).toInt - parallelStates)) {
      val dummyKeyValuePairs = Helper.dummyKeyValuePairs(commonElements + parallelStates)

      val crdt: DeltaAddWinsLastWriterWinsMap[String, String] =
        new DeltaAddWinsLastWriterWinsMap[String, String]("0")
      var currentDot = LamportClock(0, "0")

      val untrustedReplica = new UntrustedReplica() {
        override protected def prune(): Unit = {}

        override protected def disseminate(encryptedState: EncryptedDeltaGroup): Unit = {}

        def size(): Int = {
          encryptedDeltaGroupStore.map { delta =>
            delta.stateCiphertext.length + delta.serialDottedVersionVector.length
          }.sum
        }

        def decryptAndWriteDecryptedNotReserialized(): Unit = {
          val os = Files.newOutputStream(debugOutDir.resolve("decrypted-deltas" + encryptedDeltaGroupStore.size))
          val printWriter = new PrintWriter(os)
          encryptedDeltaGroupStore.foreach(encDeltaGroup => {
            printWriter.print(new String(aead.decrypt(encDeltaGroup.stateCiphertext, encDeltaGroup.serialDottedVersionVector)))
            printWriter.print('|')
            printWriter.println(new String(encDeltaGroup.serialDottedVersionVector))
          })
          printWriter.close()
        }

        def decryptAndWriteDeltasToFile(): Unit = {
          val os = Files.newOutputStream(debugOutDir.resolve("deltas-untrusted-delta-replica-" + encryptedDeltaGroupStore.size))
          val printWriter = new PrintWriter(os)
          encryptedDeltaGroupStore.foreach(encDeltaGroup => printWriter.println(encDeltaGroup.decrypt(aead)))
          printWriter.close()
        }

        def decryptAndWriteStateToFile(): Unit = {
          val os = Files.newOutputStream(debugOutDir.resolve("state-untrusted-delta-replica-" + encryptedDeltaGroupStore.size))
          val printWriter = new PrintWriter(os)
          val crdt = decrypt(aead)
          printWriter.write(writeToString(crdt.state))
          printWriter.close()
        }

        def decrypt(aead: Aead): DeltaAddWinsLastWriterWinsMap[String, String] = {
          val crdt = new DeltaAddWinsLastWriterWinsMap[String, String]("")
          encryptedDeltaGroupStore.map { encDeltaGroup =>
            encDeltaGroup.decrypt(aead)
          }.foreach { decDeltaGroup =>
            crdt.merge(decDeltaGroup.deltaGroup)
          }

          crdt
        }
      }

      for (i <- 0 until commonElements) {
        val entry = dummyKeyValuePairs(i)
        val delta = crdt.putDelta(entry._1, entry._2)
        currentDot = currentDot.advance(i.toString)
        val encDelta = DecryptedDeltaGroup(delta, Set(currentDot)).encrypt(aead)
        untrustedReplica.receive(encDelta)
      }

      var unmergedDeltas = List.empty[DeltaAddWinsLastWriterWinsMap.StateType[String, String]]
      for (replicaId <- 1 to parallelStates) {
        val entry = dummyKeyValuePairs(commonElements + replicaId - 1)
        val replicaSpecificCrdt = new DeltaAddWinsLastWriterWinsMap[String, String](replicaId.toString, crdt.state, crdt.deltas)
        val delta = replicaSpecificCrdt.putDelta(entry._1, entry._2)
        unmergedDeltas = unmergedDeltas :+ delta
        val dot = LamportClock(1, replicaId.toString)
        val encState = DecryptedDeltaGroup(delta, Set(dot)).encrypt(aead)
        untrustedReplica.receive(encState)
      }

      val mergedCrdt = new DeltaAddWinsLastWriterWinsMap[String, String]("0", crdt.state, crdt.deltas)
      unmergedDeltas.foreach(delta => mergedCrdt.merge(delta))
      val serializedDecryptedMergedState = writeToArray(mergedCrdt.state)

      if (debug) {
        val bos = new ByteArrayOutputStream()
        new ObjectOutputStream(bos).writeObject(mergedCrdt.state)
        println(bos.size())

        if (parallelStates == 1) {
          untrustedReplica.decryptAndWriteDeltasToFile()
          untrustedReplica.decryptAndWriteStateToFile()
          untrustedReplica.decryptAndWriteDecryptedNotReserialized()
        }
      }

      val mergedSize = serializedDecryptedMergedState.length
      val csvLine = s"$parallelStates,$commonElements,${parallelStates + commonElements},${untrustedReplica.size()},$mergedSize"
      println(csvLine)
      csvFile.println(csvLine)
    }
  }

  csvFile.close()
}
