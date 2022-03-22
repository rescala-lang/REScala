package de.ckuessner
package encrdt.encrypted.deltabased

import encrdt.causality.DotStore.Dot

import scala.collection.mutable

abstract class UntrustedReplica(initialDeltaGroups: Set[EncryptedDeltaGroup] = Set.empty) extends Replica {
  protected var dottedVersionVector: mutable.Set[Dot] = mutable.Set.empty
  protected var encryptedDeltaGroupStore: mutable.Set[EncryptedDeltaGroup] = mutable.Set.from(initialDeltaGroups)

  override def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    prune(encryptedDeltaGroup)

    dottedVersionVector.addAll(encryptedDeltaGroup.dottedVersionVector)
    encryptedDeltaGroupStore.add(encryptedDeltaGroup)
  }

  protected def prune(receivedEncryptedDeltaGroup: EncryptedDeltaGroup): Unit
}
