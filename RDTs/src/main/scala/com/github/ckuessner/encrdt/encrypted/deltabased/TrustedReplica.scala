package com.github.ckuessner.encrdt.encrypted.deltabased

import com.github.ckuessner.encrdt.causality.{CausalContext, Dot}
import com.github.ckuessner.encrdt.crdts.interfaces.Crdt
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.Aead

abstract class TrustedReplica[T](val replicaId: String, val crdt: Crdt[T], private val aead: Aead)(using
    stateJsonCodec: JsonValueCodec[T],
    dotSetJsonCodec: JsonValueCodec[CausalContext]
) extends Replica {

  protected var dottedVersionVector: CausalContext = CausalContext()

  private var lastDot = Dot(0, replicaId)

  protected def nextDot(): Dot = {
    lastDot = lastDot.advance(replicaId)
    lastDot
  }

  def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    val decryptedState: DecryptedDeltaGroup[T] = encryptedDeltaGroup.decrypt(aead)
    synchronized {
      dottedVersionVector = dottedVersionVector.merged(decryptedState.dottedVersionVector)
      // TODO: Non-causally consistent unless underlying CRDT handles causal consistency
      crdt.merge(decryptedState.deltaGroup)
    }
  }

  def localChange(state: T): Unit = {
    val eventDot = nextDot()
    dottedVersionVector.add(eventDot)
    val encryptedDelta = DecryptedDeltaGroup(state, CausalContext(eventDot)).encrypt(aead)
    disseminate(encryptedDelta)
  }
}
