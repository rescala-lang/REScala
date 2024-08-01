package benchmarks.encrdt.statebased

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.LocalUid
import rdts.time.VectorClock
import replication.Aead

abstract class TrustedReplica[T](val localReplicaId: LocalUid, private val aead: Aead)(using
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
