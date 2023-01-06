package benchmarks.encrdt.mock

import benchmarks.encrdt.mock.SecureToDoListClient.{ToDoMapLattice, mergeDecryptedDeltas}
import benchmarks.encrdt.todolist.ToDoEntry
import com.google.crypto.tink.Aead
import kofre.time.Dots
import kofre.deprecated.containers.DeltaAddWinsLastWriterWinsMap.DeltaAddWinsLastWriterWinsMapLattice
import benchmarks.encrdt.Codecs._
import encrdtlib.encrypted.deltabased.{DecryptedDeltaGroup, EncryptedDeltaGroup, TrustedReplica, UntrustedReplica}
import kofre.deprecated.containers.DeltaAddWinsLastWriterWinsMap
import kofre.dotted.{DotMap, Dotted}

import java.util.UUID
import scala.collection.mutable

class SecureToDoListClient(
    replicaId: String,
    crdt: DeltaAddWinsLastWriterWinsMap[UUID, ToDoEntry],
    aead: Aead,
    private val intermediary: UntrustedReplica
) extends TrustedReplica[ToDoMapLattice](replicaId, crdt.merge, aead) with ToDoListClient {

  private val uuidToDeltaGroupMap: mutable.Map[UUID, DecryptedDeltaGroup[ToDoMapLattice]] = mutable.Map.empty
  private var cleanupDeltaGroup: DecryptedDeltaGroup[ToDoMapLattice] =
    DecryptedDeltaGroup(Dotted(DotMap.empty, Dots.empty), Dots.empty)

  private var _disseminatedDataInBytes: Long = 0
  def disseminatedDataInBytes: Long          = _disseminatedDataInBytes

  private var _disseminatedDataAddition: Long   = 0
  private var _disseminatedDataCompletion: Long = 0
  private var _disseminatedDataRemoval: Long    = 0

  def disseminationStats: DisseminationStats = DisseminationStats(
    _disseminatedDataInBytes,
    _disseminatedDataAddition,
    _disseminatedDataCompletion,
    _disseminatedDataRemoval
  )

  override protected def disseminate(encryptedState: EncryptedDeltaGroup): Unit = {
    _disseminatedDataInBytes += encryptedState.stateCiphertext.length + encryptedState.serialDottedVersionVector.length
    intermediary.receive(encryptedState)
  }

  def completeToDoItem(uuid: UUID): Unit = {
    val _disseminatedBefore = _disseminatedDataInBytes
    val delta = crdt.putDelta(
      uuid,
      crdt.get(uuid).get.copy(completed = true)
    )
    localChangeOptimized(delta, uuid)
    _disseminatedDataCompletion += _disseminatedDataInBytes - _disseminatedBefore
  }

  def addToDoItem(uuid: UUID, toDoEntry: ToDoEntry): Unit = {
    val _disseminatedBefore = _disseminatedDataInBytes
    val delta               = crdt.putDelta(uuid, toDoEntry)
    localChangeOptimized(delta, uuid)
    _disseminatedDataAddition += _disseminatedDataInBytes - _disseminatedBefore
  }

  def removeToDoItems(uuids: Seq[UUID]): Unit = {
    val _disseminatedBefore = _disseminatedDataInBytes
    val delta               = crdt.removeAllDelta(uuids)
    localChangeRemovalOptimized(delta, uuids)
    _disseminatedDataRemoval += _disseminatedDataInBytes - _disseminatedBefore
  }

  def localChangeRemovalOptimized(delta: ToDoMapLattice, removedUuids: Seq[UUID]): Unit = {
    val eventDot = nextDot()
    dottedVersionVector.add(eventDot)

    // Merge all deltas referring to any of the removed uuids and remove old deltas (without replacement)
    val mergedOldDeltas = removedUuids.flatMap(uuidToDeltaGroupMap.remove).reduce((l, r) =>
      mergeDecryptedDeltas(l, r)
    )

    val newCleanupDelta = mergeDecryptedDeltas(
      mergeDecryptedDeltas(mergedOldDeltas, cleanupDeltaGroup),
      DecryptedDeltaGroup(delta, Dots.single(eventDot))
    )
    cleanupDeltaGroup = newCleanupDelta

    encryptAndDisseminate(newCleanupDelta)
  }

  def localChangeOptimized(delta: ToDoMapLattice, uuid: UUID): Unit = {
    val eventDot = nextDot()
    dottedVersionVector.add(eventDot)

    // Merge old delta referring to uuid
    val newDelta = uuidToDeltaGroupMap.get(uuid) match {
      case Some(oldUuidDeltaGroup) => DecryptedDeltaGroup(
          DeltaAddWinsLastWriterWinsMap.deltaAddWinsMapLattice.merge(oldUuidDeltaGroup.deltaGroup, delta),
          oldUuidDeltaGroup.dottedVersionVector.add(eventDot)
        )

      case None => DecryptedDeltaGroup(delta, Dots.single(eventDot))
    }
    uuidToDeltaGroupMap.put(uuid, newDelta)

    encryptAndDisseminate(newDelta)
  }

  protected def encryptAndDisseminate(newDeltaGroup: DecryptedDeltaGroup[ToDoMapLattice]): Unit = {
    disseminate(newDeltaGroup.encrypt(aead)(stateJsonCodec, dotSetJsonCodec))
  }

  // call localChange instead of localChangeOptimized
  override def localChange(delta: ToDoMapLattice): Unit = ???
}

object SecureToDoListClient {
  type ToDoMapLattice = DeltaAddWinsLastWriterWinsMapLattice[UUID, ToDoEntry]

  private def mergeDecryptedDeltas(
      left: DecryptedDeltaGroup[ToDoMapLattice],
      right: DecryptedDeltaGroup[ToDoMapLattice]
  ): DecryptedDeltaGroup[ToDoMapLattice] = {
    DecryptedDeltaGroup.decryptedDeltaGroupSemiLattice[ToDoMapLattice](
      DeltaAddWinsLastWriterWinsMap.deltaAddWinsMapLattice
    ).merge(left, right)
  }
}

case class DisseminationStats(total: Long, addition: Long, completion: Long, removal: Long) {
  def -(other: DisseminationStats): DisseminationStats = other match {
    case DisseminationStats(otherTotal, otherAddition, otherCompletion, otherRemoval) =>
      DisseminationStats(
        total - otherTotal,
        addition - otherAddition,
        completion - otherCompletion,
        removal - otherRemoval
      )
  }
}
