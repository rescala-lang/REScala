package com.github.ckuessner.encrdt.encrypted.statebased

import com.github.ckuessner.encrdt.causality.VectorClock
import com.github.ckuessner.encrdt.crdts.interfaces.Crdt
import com.github.ckuessner.encrdt.lattices.{MultiValueRegisterLattice, SemiLattice}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.google.crypto.tink.Aead

import scala.util.{Failure, Success, Try}

class EncryptedCrdt(initialState: MultiValueRegisterLattice[EncryptedState] = MultiValueRegisterLattice(Map.empty))
    extends Crdt[MultiValueRegisterLattice[EncryptedState]] {

  private var _state = initialState

  def state: MultiValueRegisterLattice[EncryptedState] = _state

  def currentTime: VectorClock =
    if (state.versions.isEmpty) VectorClock()
    else state.versions.keys.reduce((a, b) => a.merged(b))

  def unseal[T: SemiLattice](
      aead: Aead
  )(using tJsonValueCodec: JsonValueCodec[T], vcJsonCodec: JsonValueCodec[VectorClock]): Try[DecryptedState[T]] =
    state.versions.values.map { (encState: EncryptedState) =>
      Try {
        encState.decrypt[T](aead)
      }
    } reduce ((leftTry: Try[DecryptedState[T]], rightTry: Try[DecryptedState[T]]) => {
      (leftTry, rightTry) match {
        case (Success(left), Success(right)) =>
          Success(
            DecryptedState(
              SemiLattice[T].merged(left.state, right.state),
              left.versionVector.merged(right.versionVector)
            )
          )
        case (Failure(e), _) => Failure(e)
        case (_, Failure(e)) => Failure(e)
      }
    })

  override def merge(other: MultiValueRegisterLattice[EncryptedState]): Unit = {
    _state = SemiLattice[MultiValueRegisterLattice[EncryptedState]].merged(_state, other)
  }
}

case class EncryptedState(stateCiphertext: Array[Byte], serialVersionVector: Array[Byte]) {
  def versionVector(using jsonValueCodec: JsonValueCodec[VectorClock]): VectorClock = readFromArray(
    serialVersionVector
  )

  def decrypt[T](
      aead: Aead
  )(using tJsonCodec: JsonValueCodec[T], vcJsonCodec: JsonValueCodec[VectorClock]): DecryptedState[T] = {
    val plainText     = aead.decrypt(stateCiphertext, serialVersionVector)
    val state         = readFromArray[T](plainText)
    val versionVector = readFromArray[VectorClock](serialVersionVector)
    DecryptedState(state, versionVector)
  }
}

case class DecryptedState[T](state: T, versionVector: VectorClock) {
  def encrypt(
      aead: Aead
  )(using tJsonCodec: JsonValueCodec[T], vcJsonCodec: JsonValueCodec[VectorClock]): EncryptedState = {
    val serialVectorClock = writeToArray(versionVector)
    val stateCipherText = aead.encrypt(
      writeToArray(state),
      serialVectorClock
    )
    EncryptedState(stateCipherText, serialVectorClock)
  }
}

object DecryptedState {
  given lattice[T](using tLattice: SemiLattice[T]): SemiLattice[DecryptedState[T]] = (left, right) => {
    DecryptedState(SemiLattice[T].merged(left.state, right.state), left.versionVector.merged(right.versionVector))
  }
}
