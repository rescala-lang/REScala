package benchmarks.encrdt.mock

import benchmarks.encrdt.Codecs
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.google.crypto.tink.Aead
import kofre.time.Dots

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import benchmarks.encrdt.Codecs.dotSetCodec
import benchmarks.encrdt.idFromString
import encrdtlib.container.DeltaAddWinsLastWriterWinsMap
import encrdtlib.encrypted.deltabased.{EncryptedDeltaGroup, UntrustedReplica}

class UntrustedDeltaBasedReplicaMock extends UntrustedReplica {
  override protected def prune(encryptedDeltaGroup: EncryptedDeltaGroup): Unit  = {}
  override protected def disseminate(encryptedState: EncryptedDeltaGroup): Unit = {}

  def getCausalContext: Dots = dottedVersionVector

  def size(): Long = {
    encryptedDeltaGroupStore.toList.map { delta =>
      delta.stateCiphertext.length.toLong + delta.serialDottedVersionVector.length.toLong
    }.sum
  }

  def decryptAndWriteRawDeltasToFile(aead: Aead, outFilepath: Path): Unit = {
    val os          = Files.newOutputStream(outFilepath)
    val printWriter = new PrintWriter(os)
    encryptedDeltaGroupStore.foreach(encDeltaGroup => {
      printWriter.print(new String(aead.decrypt(
        encDeltaGroup.stateCiphertext,
        encDeltaGroup.serialDottedVersionVector
      )))
      printWriter.print('|')
      printWriter.println(new String(encDeltaGroup.serialDottedVersionVector))
    })
    printWriter.close()
  }

  def decryptAndWriteDeltasToFile(aead: Aead, outFilePath: Path): Unit = {
    val os          = Files.newOutputStream(outFilePath)
    val printWriter = new PrintWriter(os)
    encryptedDeltaGroupStore.foreach(encDeltaGroup => printWriter.println(encDeltaGroup.decrypt(aead)))
    printWriter.close()
  }

  def decryptAndWriteStateToFile(aead: Aead, outFilePath: Path): Unit = {
    val os          = Files.newOutputStream(outFilePath)
    val printWriter = new PrintWriter(os)
    val crdt        = decrypt(aead)
    printWriter.write(writeToString(crdt.state)(Codecs.deltaAwlwwmapJsonCodec))
    printWriter.close()
  }

  def decrypt(aead: Aead): DeltaAddWinsLastWriterWinsMap[String, String] = {
    val crdt = new DeltaAddWinsLastWriterWinsMap[String, String]("")
    encryptedDeltaGroupStore.map { encDeltaGroup =>
      encDeltaGroup.decrypt(aead)(Codecs.deltaAwlwwmapJsonCodec)
    }.foreach { decDeltaGroup =>
      crdt.merge(decDeltaGroup.deltaGroup)
    }

    crdt
  }

  def copy(): UntrustedDeltaBasedReplicaMock = {
    val obj = new UntrustedDeltaBasedReplicaMock()
    obj.encryptedDeltaGroupStore = encryptedDeltaGroupStore
    obj.dottedVersionVector = dottedVersionVector
    obj
  }
}
