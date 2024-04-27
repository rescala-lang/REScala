package lofi_acl.encrdt

import lofi_acl.ardt.base.Bottom
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.Aead
import rdts.base.Lattice
import rdts.syntax.LocalUid
import rdts.time.{Dot, Dots}

abstract class TrustedReplica[T: Lattice: Bottom: JsonValueCodec](val replicaId: LocalUid, private val aead: Aead)(using
    dotSetJsonCodec: JsonValueCodec[Dots]
) extends Replica {
  private var crdt: T = Bottom[T].empty

  protected var dottedVersionVector: Dots = Dots.empty

  private var lastDot = Dot(replicaId.uid, 0)

  protected def nextDot(): Dot = {
    lastDot = lastDot.advance
    lastDot
  }

  def receive(encryptedDeltaGroup: EncryptedDeltaGroup): Unit = {
    val decryptedState: DecryptedDeltaGroup[T] = encryptedDeltaGroup.decrypt(aead)
    synchronized {
      dottedVersionVector = dottedVersionVector.union(decryptedState.dottedVersionVector)
      // TODO: Non-causally consistent unless underlying CRDT handles causal consistency
      crdt = Lattice.merge(crdt, decryptedState.deltaGroup)
    }
  }

  def localChange(delta: T): Unit = {
    val eventDot = nextDot()
    dottedVersionVector.add(eventDot)
    val encryptedDelta = DecryptedDeltaGroup(delta, Dots.single(eventDot)).encrypt(aead)
    disseminate(encryptedDelta)
  }
}
