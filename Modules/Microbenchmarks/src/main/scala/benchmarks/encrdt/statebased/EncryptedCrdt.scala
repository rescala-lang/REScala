package benchmarks.encrdt.statebased

import benchmarks.encrdt.statebased.DecryptedState.vectorClockJsonCodec
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.base.Lattice
import rdts.base.Lattice.*
import rdts.datatypes.alternatives.MultiValueRegister
import rdts.time.VectorClock
import replication.Aead
import replication.JsoniterCodecs.given

import scala.util.{Failure, Success, Try}

class EncryptedCrdt(initialState: MultiValueRegister[EncryptedState] = MultiValueRegister(Map.empty)) {

  private var _state = initialState

  def state: MultiValueRegister[EncryptedState] = _state

  def currentTime: VectorClock =
    if state.versions.isEmpty then VectorClock.zero
    else state.versions.keys.reduce((a, b) => a.merge(b))

  def unseal[T: Lattice](aead: Aead)(using jsonValueCodec: JsonValueCodec[T]): Try[DecryptedState[T]] =
    state.versions.values.map { (encState: EncryptedState) =>
      Try {
        encState.decrypt[T](aead)(using jsonValueCodec)
      }
    } reduce ((leftTry: Try[DecryptedState[T]], rightTry: Try[DecryptedState[T]]) => {
      (leftTry, rightTry) match {
        case (Success(left), Success(right)) => Success(
            DecryptedState(Lattice[T].merge(left.state, right.state), left.versionVector.merge(right.versionVector))
          )
        case (Failure(e), _) => Failure(e)
        case (_, Failure(e)) => Failure(e)
      }
    })

  def merge(other: MultiValueRegister[EncryptedState]): Unit = {
    _state = Lattice[MultiValueRegister[EncryptedState]].merge(_state, other)
  }
}

case class EncryptedState(stateCiphertext: Array[Byte], serialVersionVector: Array[Byte]) {
  lazy val versionVector: VectorClock = readFromArray[VectorClock](serialVersionVector)

  def decrypt[T](aead: Aead)(using tJsonCodec: JsonValueCodec[T]): DecryptedState[T] = {
    val plainText     = aead.decrypt(stateCiphertext, serialVersionVector).get
    val state         = readFromArray[T](plainText)
    val versionVector = readFromArray[VectorClock](serialVersionVector)
    DecryptedState(state, versionVector)
  }
}

object EncryptedState {
  given encStateJsonCodec: JsonValueCodec[EncryptedState] = JsonCodecMaker.make
}

case class DecryptedState[T](state: T, versionVector: VectorClock) {
  def encrypt(aead: Aead)(using tJsonCodec: JsonValueCodec[T]): EncryptedState = {
    val serialVectorClock = writeToArray(versionVector)
    val stateCipherText = aead.encrypt(
      writeToArray(state),
      serialVectorClock
    )
    EncryptedState(stateCipherText, serialVectorClock)
  }
}

object DecryptedState {
  given vectorClockJsonCodec: JsonValueCodec[VectorClock] = JsonCodecMaker.make

  given lattice[T](using tLattice: Lattice[T]): Lattice[DecryptedState[T]] = (left, right) => {
    DecryptedState(Lattice[T].merge(left.state, right.state), left.versionVector.merge(right.versionVector))
  }
}
