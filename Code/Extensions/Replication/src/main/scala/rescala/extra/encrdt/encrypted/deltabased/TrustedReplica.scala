package rescala.extra.encrdt.encrypted.deltabased


import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.Aead
import de.ckuessner.encrdt.crdts.interfaces.Crdt
import kofre.causality.{CausalContext, Dot}
import kofre.base.Lattice.Operators

abstract class TrustedReplica[T](val replicaId: String, val crdt: Crdt[T], private val aead: Aead)(
    implicit val stateJsonCodec: JsonValueCodec[T],
    implicit val dotSetJsonCodec: JsonValueCodec[CausalContext]
) extends Replica {

  protected var dottedVersionVector: CausalContext = CausalContext.empty

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
    crdt.merge(decryptedState.deltaGroup)
  }

  def localChange(state: T): Unit = {
    val eventDot = nextDot()
    dottedVersionVector.add(eventDot)
    val encryptedDelta = DecryptedDeltaGroup(state, CausalContext.single(eventDot)).encrypt(aead)
    disseminate(encryptedDelta)
  }
}
