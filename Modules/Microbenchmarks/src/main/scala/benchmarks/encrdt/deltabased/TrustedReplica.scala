package benchmarks.encrdt.deltabased

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.LocalUid
import rdts.time.{Dot, Dots}
import replication.Aead
abstract class TrustedReplica[T](val replicaId: LocalUid, mutate: T => Unit, private val aead: Aead|Null)(
    implicit
    val stateJsonCodec: JsonValueCodec[T],
    val dotSetJsonCodec: JsonValueCodec[Dots]
) extends Replica {

  protected var dottedVersionVector: Dots = Dots.empty

  private var lastDot = Dot(replicaId.uid, 0)

  protected def nextDot(): Dot = {
    lastDot = lastDot.advance
    lastDot
  }

  def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    assert(aead ne null, "aead was null?")
    val decryptedState: DecryptedDeltaGroup[T] = encryptedDeltaGroup.decrypt(aead)
    dottedVersionVector = dottedVersionVector.merge(decryptedState.dottedVersionVector)
    // TODO: synchronize
    // TODO: Non-causally consistent unless underlying CRDT handles causal consistency
    mutate(decryptedState.deltaGroup)
  }

  def localChange(state: T): Unit = {
    assert(aead ne null, "aead was null?")
    val eventDot = nextDot()
    dottedVersionVector.add(eventDot)
    val encryptedDelta = DecryptedDeltaGroup(state, Dots.single(eventDot)).encrypt(aead)
    disseminate(encryptedDelta)
  }
}
