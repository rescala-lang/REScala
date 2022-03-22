package de.ckuessner
package encrdt.encrypted.deltabased

import encrdt.causality.DotStore.{Dot, DotSet}
import encrdt.causality.{CausalContext, LamportClock}
import encrdt.crdts.interfaces.Crdt
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.Aead

import scala.collection.mutable

abstract class TrustedReplica[T](val replicaId: String,
                                 val crdt: Crdt[T],
                                 private val aead: Aead)
                                (implicit val stateJsonCodec: JsonValueCodec[T],
                                 implicit val dotSetJsonCodec: JsonValueCodec[CausalContext]
                                ) extends Replica {

  protected var dottedVersionVector: CausalContext = CausalContext()

  private var lastDot = LamportClock(0, replicaId)

  protected def nextDot(): Dot = {
    lastDot = lastDot.advance(replicaId)
    lastDot
  }

  def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    val decryptedState: DecryptedDeltaGroup[T] = encryptedDeltaGroup.decrypt(aead)
    dottedVersionVector =  dottedVersionVector.merged(decryptedState.dottedVersionVector)
    // TODO: synchronize
    // TODO: Non-causally consistent unless underlying CRDT handles causal consistency
    crdt.merge(decryptedState.deltaGroup)
  }

  def localChange(state: T): Unit = {
    val eventDot = nextDot()
    dottedVersionVector.add(eventDot)
    val encryptedDelta = DecryptedDeltaGroup(state, Set(eventDot)).encrypt(aead)
    disseminate(encryptedDelta)
  }
}
