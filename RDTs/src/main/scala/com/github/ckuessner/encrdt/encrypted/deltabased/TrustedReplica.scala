package com.github.ckuessner.encrdt.encrypted.deltabased

import com.github.ckuessner.ardt.base.{Bottom, Lattice}
import com.github.ckuessner.ardt.causality.{CausalContext, Dot}
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.Aead

abstract class TrustedReplica[T: Lattice: Bottom: JsonValueCodec](val replicaId: String, private val aead: Aead)(using
    dotSetJsonCodec: JsonValueCodec[CausalContext]
) extends Replica {
  private var crdt: T = Bottom[T].empty

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
      crdt = Lattice.merge(crdt, decryptedState.deltaGroup)
    }
  }

  def localChange(delta: T): Unit = {
    val eventDot = nextDot()
    dottedVersionVector.add(eventDot)
    val encryptedDelta = DecryptedDeltaGroup(delta, CausalContext(eventDot)).encrypt(aead)
    disseminate(encryptedDelta)
  }
}
