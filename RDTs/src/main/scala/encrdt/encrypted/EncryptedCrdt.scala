package de.ckuessner
package encrdt.encrypted

import encrdt.causality.VectorClock

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.google.crypto.tink.Aead
import de.ckuessner.encrdt.lattices.{MultiValueRegisterLattice, SemiLattice}

import scala.util.{Failure, Success, Try}

class EncryptedCrdt[T: SemiLattice](val aead: Aead,
                                    replicaId: String,
                                    initialState: MultiValueRegisterLattice[Array[Byte]] = MultiValueRegisterLattice(Map.empty)) {

  private var _state = initialState

  def state: MultiValueRegisterLattice[Array[Byte]] = _state

  def currentTime: VectorClock =
    if (state.versions.isEmpty) VectorClock()
    else state.versions.keys.reduce((a, b) => a.merged(b))

  def unseal(implicit jsonValueCodec: JsonValueCodec[T]): Try[T] =
    state.versions.values.map { ciphertext: Array[Byte] =>
      Try {
        // Ignore associated data for now
        aead.decrypt(ciphertext, new Array[Byte](0))
      } map {
        bytes => readFromArray[T](bytes)
      }
    } reduce ((leftTry: Try[T], rightTry: Try[T]) => {
      (leftTry, rightTry) match {
        case (Success(left), Success(right)) => Success(SemiLattice[T].merged(left, right))
        case (f@Failure(_), _) => f
        case (_, f@Failure(_)) => f
      }
    })

  def update(newPlainTextState: T)(implicit jsonValueCodec: JsonValueCodec[T]): Try[Unit] = Try {
    val ciphertextBytes = aead.encrypt(writeToArray(newPlainTextState), new Array[Byte](0))
    _state = MultiValueRegisterLattice(Map(currentTime.advance(replicaId) -> ciphertextBytes))
  }
}

object EncryptedCrdt {
  def from[T: SemiLattice](crdtState: T, replicaId: String, aead: Aead)(implicit codec: JsonValueCodec[T]): Try[EncryptedCrdt[T]] = {
    val encCrdt = new EncryptedCrdt[T](aead, replicaId)
    encCrdt.update(crdtState) match {
      case Failure(exception) => Failure[EncryptedCrdt[T]](exception)
      case Success(_) => Try(encCrdt)
    }
  }
}