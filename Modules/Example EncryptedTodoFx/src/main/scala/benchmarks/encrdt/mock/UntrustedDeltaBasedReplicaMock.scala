package benchmarks.encrdt.mock

import benchmarks.encrdt.Codecs.given
import benchmarks.encrdt.{Codecs, idFromString, localidFromString}
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.google.crypto.tink.Aead
import encrdtlib.container.DeltaAWLWWMContainer
import encrdtlib.encrypted.deltabased.{EncryptedDeltaGroup, UntrustedReplica}
import rdts.time.Dots

import java.io.PrintWriter
import java.nio.file.{Files, Path}

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

  def decryptAndWriteDeltasToFile(aead: replication.Aead, outFilePath: Path): Unit = {
    val os          = Files.newOutputStream(outFilePath)
    val printWriter = new PrintWriter(os)
    encryptedDeltaGroupStore.foreach(encDeltaGroup => printWriter.println(encDeltaGroup.decrypt[Dots](aead)))
    printWriter.close()
  }

  def decryptAndWriteStateToFile(aead: replication.Aead, outFilePath: Path): Unit = {
    val os          = Files.newOutputStream(outFilePath)
    val printWriter = new PrintWriter(os)
    val crdt        = decrypt(aead)
    printWriter.write(writeToString(crdt.state)(Codecs.deltaAwlwwmapJsonCodec))
    printWriter.close()
  }

  def decrypt(aead: replication.Aead): DeltaAWLWWMContainer[String, String] = {
    val crdt = new DeltaAWLWWMContainer[String, String]("")
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
