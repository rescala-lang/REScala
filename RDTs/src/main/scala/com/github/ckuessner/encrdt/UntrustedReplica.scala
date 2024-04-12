package com.github.ckuessner.encrdt

import com.github.ckuessner.ardt.causality.CausalContext
import scala.collection.mutable

abstract class UntrustedReplica(initialDeltaGroups: Set[EncryptedDeltaGroup] = Set.empty) extends Replica {
  protected var dottedVersionVector: CausalContext                         = CausalContext()
  protected var encryptedDeltaGroupStore: mutable.Set[EncryptedDeltaGroup] = mutable.Set.from(initialDeltaGroups)

  override def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    prune(encryptedDeltaGroup)

    dottedVersionVector = dottedVersionVector.merged(encryptedDeltaGroup.dottedVersionVector)
    encryptedDeltaGroupStore.add(encryptedDeltaGroup)
  }

  protected def prune(receivedEncryptedDeltaGroup: EncryptedDeltaGroup): Unit
}
