package de.ckuessner
package encrdt.encrypted.deltabased

import encrdt.causality.DotSetPartialOrdering
import encrdt.causality.DotStore.Dot

import scala.collection.mutable

trait DeltaPruning {
  protected var dottedVersionVector: mutable.Set[Dot]
  protected var encryptedDeltaGroupStore: mutable.Set[EncryptedDeltaGroup]

  protected def prune(receivedDeltaGroup: EncryptedDeltaGroup): Unit = {
    encryptedDeltaGroupStore.filterInPlace(subsumedDeltaGroup =>
      !DotSetPartialOrdering.lteq(
        subsumedDeltaGroup.dottedVersionVector,
        receivedDeltaGroup.dottedVersionVector
      )
    )
  }
}
