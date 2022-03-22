package de.ckuessner
package encrdt.encrypted.deltabased

import encrdt.causality.DotStore.DotSet
import encrdt.lattices.SemiLattice

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import com.google.crypto.tink.Aead

case class DecryptedDeltaGroup[T](deltaGroup: T, dottedVersionVector: DotSet) {
  def encrypt(aead: Aead)
             (implicit tJsonCodec: JsonValueCodec[T], dotSetJsonCodec: JsonValueCodec[DotSet]): EncryptedDeltaGroup = {

    val serialDeltaGroup = writeToArray(deltaGroup)
    val serialDottedVersionVector = writeToArray(dottedVersionVector)
    val deltaGroupCipherText = aead.encrypt(serialDeltaGroup, serialDottedVersionVector)
    EncryptedDeltaGroup(deltaGroupCipherText, serialDottedVersionVector)
  }
}

object DecryptedDeltaGroup {
  implicit def decryptedDeltaGroupSemiLattice[T](implicit tLattice: SemiLattice[T]): SemiLattice[DecryptedDeltaGroup[T]] = (l, r) =>
    DecryptedDeltaGroup(SemiLattice[T].merged(l.deltaGroup, r.deltaGroup), l.dottedVersionVector union r.dottedVersionVector)
}