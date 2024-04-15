package com.github.ckuessner.encrdt

import rdts.time.Dots

import scala.collection.mutable

abstract class UntrustedReplica(initialDeltaGroups: Set[EncryptedDeltaGroup] = Set.empty) extends Replica {
  protected var dottedVersionVector: Dots                                  = Dots.empty
  protected var encryptedDeltaGroupStore: mutable.Set[EncryptedDeltaGroup] = mutable.Set.from(initialDeltaGroups)

  override def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    prune(encryptedDeltaGroup)

    dottedVersionVector = dottedVersionVector.union(encryptedDeltaGroup.dottedVersionVector)
    val _ = encryptedDeltaGroupStore.add(encryptedDeltaGroup)
  }

  protected def prune(receivedEncryptedDeltaGroup: EncryptedDeltaGroup): Unit
}
