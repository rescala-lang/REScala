package de.ckuessner
package encrdt.experiments

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.crypto.tink.Aead

import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

class EncryptedCrdt[T: SemiLattice](val aead: Aead,
                                    initialState: EncryptedCrdtState = EncryptedCrdtState()) {

  private var _state = initialState

  def state: EncryptedCrdtState = _state

  def unseal(implicit jsonValueCodec: JsonValueCodec[T]): Try[T] =
    state.states map { ciphertext: Array[Byte] =>
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

  def appendState(newState: T)(implicit jsonValueCodec: JsonValueCodec[T]): Try[Unit] = Try {
    val cleartextBytes = writeToArray(newState)
    val ciphertextBytes = aead.encrypt(cleartextBytes, new Array[Byte](0))
    _state = state.enqueue(ciphertextBytes)
  }
}

object EncryptedCrdt {
  def from[T: SemiLattice](crdtState: T, aead: Aead)(implicit codec: JsonValueCodec[T]): Try[EncryptedCrdt[T]] = {
    val encCrdt = new EncryptedCrdt[T](aead)
    encCrdt.appendState(crdtState) match {
      case Failure(exception) => Failure[EncryptedCrdt[T]](exception)
      case Success(_) => Try(encCrdt)
    }
  }
}

case class EncryptedCrdtState(states: Queue[Array[Byte]] = Queue()) {
  def enqueue(bytes: Array[Byte]): EncryptedCrdtState = EncryptedCrdtState(states.enqueue(bytes))
}

object EncryptedCrdtState {
  implicit val jsonCodec: JsonValueCodec[EncryptedCrdtState] = JsonCodecMaker.make

  implicit val semiLattice: SemiLattice[EncryptedCrdtState] = (left: EncryptedCrdtState, right: EncryptedCrdtState) => {
    // TODO: This won't work for Optimized AWSet
    // TODO: This only makes sense for deltas (maybe use a Set and a replace Array[Byte] with an object containing meta)
    EncryptedCrdtState(left.states.enqueueAll(right.states))
  }
}