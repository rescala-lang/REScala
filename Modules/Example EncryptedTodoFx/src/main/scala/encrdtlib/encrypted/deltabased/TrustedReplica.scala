package encrdtlib.encrypted.deltabased

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.Aead
import kofre.time.{Dots, Dot}
import benchmarks.encrdt.idFromString
abstract class TrustedReplica[T](val replicaId: String, mutate: T => Unit, private val aead: Aead)(
    implicit
    val stateJsonCodec: JsonValueCodec[T],
    val dotSetJsonCodec: JsonValueCodec[Dots]
) extends Replica {

  protected var dottedVersionVector: Dots = Dots.empty

  private var lastDot = Dot(replicaId, 0)

  protected def nextDot(): Dot = {
    lastDot = lastDot.advance
    lastDot
  }

  def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    val decryptedState: DecryptedDeltaGroup[T] = encryptedDeltaGroup.decrypt(aead)
    dottedVersionVector = dottedVersionVector.merge(decryptedState.dottedVersionVector)
    // TODO: synchronize
    // TODO: Non-causally consistent unless underlying CRDT handles causal consistency
    mutate(decryptedState.deltaGroup)
  }

  def localChange(state: T): Unit = {
    val eventDot = nextDot()
    dottedVersionVector.add(eventDot)
    val encryptedDelta = DecryptedDeltaGroup(state, Dots.single(eventDot)).encrypt(aead)
    disseminate(encryptedDelta)
  }
}
