package rescala.extra.encrdt.encrypted.statebased

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.crypto.tink.Aead
import kofre.Lattice
import kofre.Lattice.Operators
import kofre.causality.VectorClock
import rescala.extra.encrdt.encrypted.statebased.DecryptedState.vectorClockJsonCodec

case class EncryptedState(stateCiphertext: Array[Byte], serialVersionVector: Array[Byte]) {
  lazy val versionVector: VectorClock = readFromArray(serialVersionVector)

  def decrypt[T](aead: Aead)(implicit tJsonCodec: JsonValueCodec[T]): DecryptedState[T] = {
    val plainText     = aead.decrypt(stateCiphertext, serialVersionVector)
    val state         = readFromArray[T](plainText)
    val versionVector = readFromArray[VectorClock](serialVersionVector)
    DecryptedState(state, versionVector)
  }
}

object EncryptedState {
  implicit val encStateJsonCodec: JsonValueCodec[EncryptedState] = JsonCodecMaker.make
}

case class DecryptedState[T](state: T, versionVector: VectorClock) {
  def encrypt(aead: Aead)(implicit tJsonCodec: JsonValueCodec[T]): EncryptedState = {
    val serialVectorClock = writeToArray(versionVector)
    val stateCipherText = aead.encrypt(
      writeToArray(state),
      serialVectorClock
    )
    EncryptedState(stateCipherText, serialVectorClock)
  }
}

object DecryptedState {
  implicit val vectorClockJsonCodec: JsonValueCodec[VectorClock] = JsonCodecMaker.make

  implicit def lattice[T](implicit tLattice: Lattice[T]): Lattice[DecryptedState[T]] = (left, right) => {
    DecryptedState(Lattice[T].merge(left.state, right.state), left.versionVector.merge(right.versionVector))
  }
}
