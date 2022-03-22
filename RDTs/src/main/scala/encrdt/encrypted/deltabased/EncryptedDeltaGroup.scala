package de.ckuessner
package encrdt.encrypted.deltabased

import encrdt.causality.DotStore.DotSet

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import com.google.crypto.tink.Aead

case class EncryptedDeltaGroup(stateCiphertext: Array[Byte], serialDottedVersionVector: Array[Byte])
                              (implicit dotSetJsonCodec: JsonValueCodec[DotSet]) {
  lazy val dottedVersionVector: DotSet = readFromArray(serialDottedVersionVector)

  def decrypt[T](aead: Aead)(implicit tJsonCodec: JsonValueCodec[T]): DecryptedDeltaGroup[T] = {
    val plainText = aead.decrypt(stateCiphertext, serialDottedVersionVector)
    val state = readFromArray[T](plainText)
    val dottedVersionVector = readFromArray[DotSet](serialDottedVersionVector)
    DecryptedDeltaGroup(state, dottedVersionVector)
  }
}
