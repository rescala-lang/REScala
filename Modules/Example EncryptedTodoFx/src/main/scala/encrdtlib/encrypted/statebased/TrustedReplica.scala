package encrdtlib.encrypted.statebased

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.Aead
import rdts.syntax.LocalUid
import rdts.time.VectorClock

abstract class TrustedReplica[T](val localReplicaId: LocalUid, private val aead: Aead)(implicit
    val stateJsonCodec: JsonValueCodec[T]
) extends Replica {

  var versionVector: VectorClock = VectorClock.zero

  def receive(encryptedState: EncryptedState): Unit = {
    val decryptedState: DecryptedState[T] = encryptedState.decrypt(aead)
    versionVector = versionVector.merge(decryptedState.versionVector)
    // TODO: synchronize
    merge(decryptedState.state)
  }

  def stateChanged(state: T): Unit = {
    versionVector = versionVector.inc(localReplicaId.uid)
    val encryptedState = DecryptedState(state, versionVector).encrypt(aead)
    disseminate(encryptedState)
  }

  protected def merge(state: T): Unit

  protected def localState(): T

}
