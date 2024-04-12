package com.github.ckuessner.encrdt.encrypted.deltabased

import com.github.ckuessner.ardt.base.Lattice
import com.github.ckuessner.ardt.causality.CausalContext
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.google.crypto.tink.Aead

case class DecryptedDeltaGroup[T](deltaGroup: T, dottedVersionVector: CausalContext) {
  def encrypt(
      aead: Aead
  )(using tJsonCodec: JsonValueCodec[T], dotSetJsonCodec: JsonValueCodec[CausalContext]): EncryptedDeltaGroup = {
    val serialDeltaGroup          = writeToArray(deltaGroup)
    val serialDottedVersionVector = writeToArray(dottedVersionVector)
    val deltaGroupCipherText      = aead.encrypt(serialDeltaGroup, serialDottedVersionVector)

    EncryptedDeltaGroup(deltaGroupCipherText, serialDottedVersionVector)
  }
}

object DecryptedDeltaGroup {
  given decryptedDeltaGroupLattice[T](using tLattice: Lattice[T]): Lattice[DecryptedDeltaGroup[T]] =
    (l, r) =>
      DecryptedDeltaGroup(
        Lattice[T].merge(l.deltaGroup, r.deltaGroup),
        l.dottedVersionVector.merged(r.dottedVersionVector)
      )
}
