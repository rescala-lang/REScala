package benchmarks.encrdt.deltabased

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToArray}
import rdts.base.Lattice
import rdts.time.Dots
import replication.Aead

case class DecryptedDeltaGroup[T](deltaGroup: T, dottedVersionVector: Dots) {
  def encrypt(aead: Aead)(using
      tJsonCodec: JsonValueCodec[T],
      dotSetJsonCodec: JsonValueCodec[Dots]
  ): EncryptedDeltaGroup = {
    val serialDeltaGroup          = writeToArray(deltaGroup)
    val serialDottedVersionVector = writeToArray(dottedVersionVector)
    val deltaGroupCipherText      = aead.encrypt(serialDeltaGroup, serialDottedVersionVector)

    EncryptedDeltaGroup(deltaGroupCipherText, serialDottedVersionVector)
  }
}

object DecryptedDeltaGroup {
  given decryptedDeltaGroupSemiLattice[T](using
      tLattice: Lattice[T]
  ): Lattice[DecryptedDeltaGroup[T]] = (l, r) =>
    DecryptedDeltaGroup(
      Lattice.merge(l.deltaGroup, r.deltaGroup),
      l.dottedVersionVector.union(r.dottedVersionVector)
    )
}
