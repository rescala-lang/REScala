package benchmarks.encrdt.mock

import benchmarks.encrdt.deltabased.{DeltaPruning, EncryptedDeltaGroup, UntrustedReplica}

class ToDoListIntermediary extends UntrustedReplica with DeltaPruning with IntermediarySizeInfo {
  def sizeInBytes: Long = {
    encryptedDeltaGroupStore.iterator.map { encDelta =>
      encDelta.serialDottedVersionVector.length.toLong + encDelta.stateCiphertext.length.toLong
    }.sum
  }

  def encDeltaCausalityInfoSizeInBytes: Long = {
    encryptedDeltaGroupStore.iterator.map(_.serialDottedVersionVector.length.toLong).sum
  }

  def rawDeltasSizeInBytes: Long = {
    encryptedDeltaGroupStore.iterator.map(_.stateCiphertext.length.toLong).sum
  }

  def numberStoredDeltas: Int = encryptedDeltaGroupStore.size

  override protected def disseminate(encryptedState: EncryptedDeltaGroup): Unit = {}
}
