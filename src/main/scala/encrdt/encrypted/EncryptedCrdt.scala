package de.ckuessner
package encrdt.encrypted

import encrdt.causality.VectorClock
import encrdt.causality.VectorClock.VectorClockOrdering
import encrdt.crdts.interfaces.Crdt
import encrdt.encrypted.EncryptedCrdt.vectorClockJsonCodec
import encrdt.lattices.{MultiValueRegisterLattice, SemiLattice}

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.crypto.tink.Aead

import scala.math.PartialOrdering
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
        aead.decrypt(ciphertext, null)
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
    val ciphertextBytes = aead.encrypt(writeToArray(newPlainTextState), null)
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

  implicit val vectorClockJsonCodec: JsonValueCodec[VectorClock] = JsonCodecMaker.make
}

sealed trait Replica {
  def receive(encryptedState: EncryptedState): Unit
  protected def disseminate(encryptedState: EncryptedState): Unit
}

abstract class TrustedReplica[T](val replicaId: String,
                                 val crdt: Crdt[T],
                                 private val aead: Aead)
                                (implicit val stateJsonCodec: JsonValueCodec[T]) extends Replica {

  var versionVector: VectorClock = VectorClock()

  def receive(encryptedState: EncryptedState): Unit = {
    val decryptedState: DecryptedState[T] = encryptedState.decrypt(aead)
    versionVector.merged(decryptedState.versionVector)
    // TODO: synchronize
    crdt.merge(decryptedState.state)
  }

  def localChange(state: T): Unit = {
    versionVector = versionVector.advance(replicaId)
    val encryptedState = DecryptedState(state, versionVector).encrypt(aead)
    disseminate(encryptedState)
  }

}

abstract class UntrustedReplica(initialStates: Set[EncryptedState]) extends Replica {
  private var stateStore: Set[EncryptedState] = initialStates
  private var versionVector: VectorClock = stateStore.map(_.versionVector).reduce((l, r) => l.merged(r))

  def decrypt[T: SemiLattice](aead: Aead)(implicit tCodec: JsonValueCodec[T]): DecryptedState[T] = {
    stateStore
      .map(encState => encState.decrypt[T](aead))
      .reduce((l, r) => DecryptedState(
        SemiLattice[T].merged(l.state, r.state),
        l.versionVector.merged(r.versionVector)))
  }

  def receive(newState: EncryptedState): Unit = {
    // Update VersionVector, if necessary
    if (!VectorClockOrdering.lteq(newState.versionVector, versionVector)) {
      versionVector = versionVector.merged(newState.versionVector)
    } else {
      // received state may already be subsumed by some state in the stateStore
      if (stateStore.exists(oldState => VectorClockOrdering.lteq(newState.versionVector, oldState.versionVector))) {
        // The newState is already subsumed by a single state in the stateStore
        return
      }
    }

    stateStore = leastUpperBound(
      stateStore.filter(oldState => VectorClockOrdering.lteq(oldState.versionVector, newState.versionVector)) + newState
    )
  }

  private def leastUpperBound(states: Set[EncryptedState]): Set[EncryptedState] = {
    if (states.size <= 1) return states

    @inline
    def subsetIsUpperBound(subset: Set[(EncryptedState, Int)], state: EncryptedState): Boolean = {
      val mergeOfAllOtherVersionVectors = subset
        .map(_._1.versionVector)
        .foldLeft(VectorClock()) { case (a: VectorClock, b: VectorClock) => a.merged(b) }

      // Check if this state is subsumed by the merged state of all other values
      VectorClockOrdering.lteq(state.versionVector, mergeOfAllOtherVersionVectors)
    }

    val indexedStates = states.zipWithIndex

    def rec(subset: Set[(EncryptedState, Int)]): Set[EncryptedState] = {
      val removable = subset.filter { case (state, index) =>
        subsetIsUpperBound(subset.filterNot(_._2 == index), state)
      }.toList

      if (removable.isEmpty) return subset.map(_._1)

      // Optimization: Don't test removal of states with a lower index of the state that was removed by caller of rec()
      // Requires that removable is traversed according to natural order
      var optimalSubtree = subset.map(_._1)
      removable.foreach { case (state, i) =>
        val subsetWithoutState = subset.filter(_._2 == i)
        if (subsetIsUpperBound(subsetWithoutState, state)) {
          val result = rec(subsetWithoutState)
          if (result.size < optimalSubtree.size) optimalSubtree = result
        }
      }

      optimalSubtree
    }

    rec(indexedStates)
  }
}

object UntrustedReplica {
  object encStatePOrd extends PartialOrdering[EncryptedState] {
    override def tryCompare(x: EncryptedState, y: EncryptedState): Option[Int] =
      VectorClockOrdering.tryCompare(x.versionVector, y.versionVector)

    override def lteq(x: EncryptedState, y: EncryptedState): Boolean =
      VectorClockOrdering.lteq(x.versionVector, y.versionVector)
  }
}

case class EncryptedState(stateCiphertext: Array[Byte], serialVersionVector: Array[Byte]) {
  lazy val versionVector: VectorClock = readFromArray(serialVersionVector)

  def decrypt[T](aead: Aead)(implicit tJsonCodec: JsonValueCodec[T]): DecryptedState[T] = {
    val plainText = aead.decrypt(stateCiphertext, serialVersionVector)
    val state = readFromArray[T](plainText)
    val versionVector = readFromArray[VectorClock](serialVersionVector)
    DecryptedState(state, versionVector)
  }
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