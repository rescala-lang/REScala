package de.ckuessner
package encrdt.encrypted.deltabased

import encrdt.causality.CausalContext

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

trait NoPruning {
  protected def prune(receivedDeltaGroup: EncryptedDeltaGroup): Unit = {}
}
