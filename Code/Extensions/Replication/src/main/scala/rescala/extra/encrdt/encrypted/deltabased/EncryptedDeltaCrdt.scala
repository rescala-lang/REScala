package rescala.extra.encrdt.encrypted.deltabased

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.crypto.tink.Aead
import kofre.dotbased.DotStore.DotSet
import rescala.extra.encrdt.encrypted.deltabased.Codecs.dotSetJsonCodec

abstract class UntrustedReplica(initialDeltaGroups: Set[EncryptedDeltaGroup] = Set.empty) {
  protected var dottedVersionVector: DotSet                        = Set.empty
  protected var encryptedDeltaGroupStore: Set[EncryptedDeltaGroup] = initialDeltaGroups

  def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    dottedVersionVector = dottedVersionVector union encryptedDeltaGroup.dottedVersionVector
    encryptedDeltaGroupStore = encryptedDeltaGroupStore + encryptedDeltaGroup

    prune()
  }

  protected def prune(): Unit
}

private object Codecs {
  implicit val dotSetJsonCodec: JsonValueCodec[DotSet] = JsonCodecMaker.make
}

case class EncryptedDeltaGroup(stateCiphertext: Array[Byte], serialDottedVersionVector: Array[Byte]) {
  lazy val dottedVersionVector: DotSet = readFromArray(serialDottedVersionVector)

  def decrypt[T](aead: Aead)(implicit tJsonCodec: JsonValueCodec[T]): DecryptedDeltaGroup[T] = {
    val plainText           = aead.decrypt(stateCiphertext, serialDottedVersionVector)
    val state               = readFromArray[T](plainText)
    val dottedVersionVector = readFromArray[DotSet](serialDottedVersionVector)
    DecryptedDeltaGroup(state, dottedVersionVector)
  }
}

case class DecryptedDeltaGroup[T](deltaGroup: T, dottedVersionVector: DotSet) {
  def encrypt(aead: Aead)(implicit tJsonCodec: JsonValueCodec[T]): EncryptedDeltaGroup = {
    val serialDeltaGroup          = writeToArray(deltaGroup)
    val serialDottedVersionVector = writeToArray(dottedVersionVector)
    val deltaGroupCipherText      = aead.encrypt(serialDeltaGroup, serialDottedVersionVector)
    EncryptedDeltaGroup(deltaGroupCipherText, serialDottedVersionVector)
  }
}
