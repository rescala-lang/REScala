package benchmarks.encrdt.mock.insecure

import benchmarks.encrdt.deltabased.{DecryptedDeltaGroup, EncryptedDeltaGroup}
import benchmarks.encrdt.mock.SecureToDoListClient.ToDoMapLattice
import benchmarks.encrdt.mock.{SecureToDoListClient, ToDoListIntermediary}
import benchmarks.encrdt.todolist.ToDoEntry
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArray
import rdts.syntax.DeltaAWLWWMContainer

import java.util.UUID

class InsecureToDoListClient(
    replicaId: String,
    crdt: DeltaAWLWWMContainer[UUID, ToDoEntry],
    untrustedReplica: ToDoListIntermediary
) extends SecureToDoListClient(replicaId, crdt, null, untrustedReplica) {
  override protected def encryptAndDisseminate(newDeltaGroup: DecryptedDeltaGroup[ToDoMapLattice]): Unit = {
    // Serialize but don't encrypt!
    val serialPlaintextDeltaGroup = writeToArray(newDeltaGroup.deltaGroup)
    val serialDottedVersionVector = writeToArray(newDeltaGroup.dottedVersionVector)

    disseminate(
      EncryptedDeltaGroup(serialPlaintextDeltaGroup, serialDottedVersionVector)
    )
  }
}
