package de.ckuessner
package encrdt.encrypted.deltabased

import encrdt.causality.{CausalContext, DotSetPartialOrdering}
import encrdt.causality.DotStore.Dot

import scala.collection.mutable

trait DeltaPruning {
  protected var dottedVersionVector: CausalContext
  protected var encryptedDeltaGroupStore: mutable.Set[EncryptedDeltaGroup]

  protected def prune(receivedDeltaGroup: EncryptedDeltaGroup): Unit = {
    encryptedDeltaGroupStore.filterInPlace(subsumedDeltaGroup =>
      !(subsumedDeltaGroup.dottedVersionVector.acc <= receivedDeltaGroup.dottedVersionVector.acc)

    )
  }
}
