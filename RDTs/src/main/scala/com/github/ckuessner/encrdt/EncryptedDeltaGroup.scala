package com.github.ckuessner.encrdt

import com.github.ckuessner.ardt.causality.CausalContext
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import com.google.crypto.tink.Aead

case class EncryptedDeltaGroup(stateCiphertext: Array[Byte], serialDottedVersionVector: Array[Byte])(using
    dotSetJsonCodec: JsonValueCodec[CausalContext]
) {
  lazy val dottedVersionVector: CausalContext = readFromArray(serialDottedVersionVector)

  def decrypt[T](aead: Aead)(using tJsonCodec: JsonValueCodec[T]): DecryptedDeltaGroup[T] = {
    val plainText           = aead.decrypt(stateCiphertext, serialDottedVersionVector)
    val state               = readFromArray[T](plainText)
    val dottedVersionVector = readFromArray[CausalContext](serialDottedVersionVector)
    DecryptedDeltaGroup(state, dottedVersionVector)
  }
}
