package com.github.ckuessner.encrdt.encrypted.statebased

import com.github.ckuessner.ardt.causality.VectorClock
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.Aead

abstract class TrustedReplica[T](val localReplicaId: String, private val aead: Aead)(using
    private val tJsonCodec: JsonValueCodec[T],
    private val vcJsonCodec: JsonValueCodec[VectorClock]
) extends Replica {

  var versionVector: VectorClock = VectorClock()

  def receive(encryptedState: EncryptedState): Unit = {
    val decryptedState: DecryptedState[T] = encryptedState.decrypt(aead)
    val versionVectorOfDecryptedState     = decryptedState.versionVector
    synchronized {
      versionVector = versionVector.merged(versionVectorOfDecryptedState)
      merge(decryptedState.state)
    }
  }

  def stateChanged(state: T): Unit = {
    versionVector = versionVector.advance(localReplicaId)
    val encryptedState = DecryptedState(state, versionVector).encrypt(aead)
    disseminate(encryptedState)
  }

  protected def merge(state: T): Unit

  protected def localState(): T

}
