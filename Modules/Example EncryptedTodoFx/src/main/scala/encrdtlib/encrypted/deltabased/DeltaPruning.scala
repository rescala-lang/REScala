package encrdtlib.encrypted.deltabased

import rdts.time.Dots

import scala.collection.mutable

trait DeltaPruning {
  protected var dottedVersionVector: Dots
  protected var encryptedDeltaGroupStore: mutable.Set[EncryptedDeltaGroup]

  protected def prune(receivedDeltaGroup: EncryptedDeltaGroup): Unit = {
    encryptedDeltaGroupStore.filterInPlace(subsumedDeltaGroup =>
      !(subsumedDeltaGroup.dottedVersionVector <= receivedDeltaGroup.dottedVersionVector)
    )
    ()
  }
}
