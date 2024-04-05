package com.github.ckuessner.encrdt.encrypted.deltabased

import com.github.ckuessner.encrdt.causality.CausalContext
import com.github.ckuessner.encrdt.lattices.SemiLattice

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.google.crypto.tink.Aead

case class DecryptedDeltaGroup[T](deltaGroup: T, dottedVersionVector: CausalContext) {
  def encrypt(aead: Aead)(implicit
      tJsonCodec: JsonValueCodec[T],
      dotSetJsonCodec: JsonValueCodec[CausalContext]
  ): EncryptedDeltaGroup = {
    val serialDeltaGroup          = writeToArray(deltaGroup)
    val serialDottedVersionVector = writeToArray(dottedVersionVector)
    val deltaGroupCipherText      = aead.encrypt(serialDeltaGroup, serialDottedVersionVector)

    EncryptedDeltaGroup(deltaGroupCipherText, serialDottedVersionVector)
  }
}

object DecryptedDeltaGroup {
  implicit def decryptedDeltaGroupSemiLattice[T](implicit
      tLattice: SemiLattice[T]
  ): SemiLattice[DecryptedDeltaGroup[T]] = (l, r) =>
    DecryptedDeltaGroup(
      SemiLattice[T].merged(l.deltaGroup, r.deltaGroup),
      l.dottedVersionVector.merged(r.dottedVersionVector)
    )
}
