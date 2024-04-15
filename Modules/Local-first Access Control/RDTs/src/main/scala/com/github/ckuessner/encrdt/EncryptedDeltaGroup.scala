package com.github.ckuessner.encrdt

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import com.google.crypto.tink.Aead
import rdts.time.Dots

case class EncryptedDeltaGroup(stateCiphertext: Array[Byte], serialDottedVersionVector: Array[Byte])(using
    dotSetJsonCodec: JsonValueCodec[Dots]
) {
  lazy val dottedVersionVector: Dots = readFromArray(serialDottedVersionVector)

  def decrypt[T](aead: Aead)(using tJsonCodec: JsonValueCodec[T]): DecryptedDeltaGroup[T] = {
    val plainText           = aead.decrypt(stateCiphertext, serialDottedVersionVector)
    val state               = readFromArray[T](plainText)
    val dottedVersionVector = readFromArray[Dots](serialDottedVersionVector)
    DecryptedDeltaGroup(state, dottedVersionVector)
  }
}
