
package encrdt.encrypted.deltabased

import encrdt.causality.DotStore.{Dot, DotSet}
import encrdt.causality.{DotStore, LamportClock}
import encrdt.crdts.interfaces.Crdt
import encrdt.encrypted.deltabased.Codecs.dotSetJsonCodec
import encrdt.lattices.SemiLattice

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.crypto.tink.Aead

class EncryptedDeltaCrdt[T: SemiLattice](val aead: Aead,
                                         replicaId: String) {

}

sealed trait Replica {
  def receive(encryptedState: EncryptedDeltaGroup): Unit

  protected def disseminate(encryptedState: EncryptedDeltaGroup): Unit
}

abstract class TrustedReplica[T](val replicaId: String,
                                 val crdt: Crdt[T],
                                 private val aead: Aead)
                                (implicit val stateJsonCodec: JsonValueCodec[T]) extends Replica {

  private var dottedVersionVector: DotSet = Set.empty

  private def nextDot(): Dot = ({
    var container = LamportClock(1, replicaId)

    def nextDotImpl: DotStore.Dot = {
      val returnedVal = container
      container = container.advance(replicaId)
      returnedVal
    }

    () => nextDotImpl
  }) ()

  def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    val decryptedState: DecryptedDeltaGroup[T] = encryptedDeltaGroup.decrypt(aead)
    dottedVersionVector = dottedVersionVector union decryptedState.dottedVersionVector
    // TODO: synchronize
    // TODO: Non-causally consistent unless underlying CRDT handles causal consistency
    crdt.merge(decryptedState.deltaGroup)
  }

  def localChange(state: T): Unit = {
    val eventDot = nextDot()
    dottedVersionVector = dottedVersionVector + eventDot
    val encryptedDelta = DecryptedDeltaGroup(state, Set(eventDot)).encrypt(aead)
    disseminate(encryptedDelta)
  }
}

abstract class UntrustedReplica(initialDeltaGroups: Set[EncryptedDeltaGroup] = Set.empty) extends Replica {
  protected var dottedVersionVector: DotSet = Set.empty
  protected var encryptedDeltaGroupStore: Set[EncryptedDeltaGroup] = initialDeltaGroups

  override def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    dottedVersionVector = dottedVersionVector union encryptedDeltaGroup.dottedVersionVector
    encryptedDeltaGroupStore = encryptedDeltaGroupStore + encryptedDeltaGroup

    prune()
  }

  protected def prune(): Unit
}

private object Codecs {
  implicit val dotSetJsonCodec: JsonValueCodec[DotSet] = JsonCodecMaker.make
}

case class EncryptedDeltaGroup(stateCiphertext: Array[Byte], serialDottedVersionVector: Array[Byte]) {
  lazy val dottedVersionVector: DotSet = readFromArray(serialDottedVersionVector)

  def decrypt[T](aead: Aead)(implicit tJsonCodec: JsonValueCodec[T]): DecryptedDeltaGroup[T] = {
    val plainText = aead.decrypt(stateCiphertext, serialDottedVersionVector)
    val state = readFromArray[T](plainText)
    val dottedVersionVector = readFromArray[DotSet](serialDottedVersionVector)
    DecryptedDeltaGroup(state, dottedVersionVector)
  }
}

case class DecryptedDeltaGroup[T](deltaGroup: T, dottedVersionVector: DotSet) {
  def encrypt(aead: Aead)(implicit tJsonCodec: JsonValueCodec[T]): EncryptedDeltaGroup = {
    val serialDeltaGroup = writeToArray(deltaGroup)
    val serialDottedVersionVector = writeToArray(dottedVersionVector)
    val deltaGroupCipherText = aead.encrypt(serialDeltaGroup, serialDottedVersionVector)
    EncryptedDeltaGroup(deltaGroupCipherText, serialDottedVersionVector)
  }
}
