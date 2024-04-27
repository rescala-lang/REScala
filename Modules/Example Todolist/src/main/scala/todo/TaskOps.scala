package todo

import rdts.base.Uid
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, LocalReplicaId}
import reactives.default.*

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.unused

// `taskrefs` is unused as a reference, but is used indierectly so this parameter serves as a requirement
// that a `taskrefs` needs to be created before taskops may be used
class TaskOps(@unused taskrefs: TaskReferences, replicaID: Uid) {

  type State = DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]

  given LocalReplicaId = replicaID

  def handleCreateTodo(createTodo: Event[String]): Fold.Branch[State] = createTodo.act { desc =>
    val taskid = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"
    TaskReferences.lookupOrCreateTaskRef(taskid, Some(TaskData(desc)))
    val taskref = TaskRef(taskid)
    current.clearDeltas().prepend(taskref)
  }

  def handleRemoveAll(removeAll: Event[Any]): Fold.Branch[State] =
    removeAll.act: _ =>
      current.clearDeltas().deleteBy { (taskref: TaskRef) =>
        val isDone = taskref.task.value.state.data.read.exists(_.done)
        // todo, move to observer, disconnect during transaction does not respect rollbacks
        if (isDone) taskref.task.disconnect()
        isDone
      }

  def handleRemove(state: State)(id: String): State = {
    state.clearDeltas().deleteBy { (taskref: TaskRef) =>
      val delete = taskref.id == id
      // todo, move to observer, disconnect during transaction does not respect rollbacks
      if (delete) taskref.task.disconnect()
      delete
    }
  }

  def handleDelta(deltaEvent: Event[Dotted[ReplicatedList[TaskRef]]]): Fold.Branch[State] =
    deltaEvent.act { delta =>
      val deltaBuffered = current

      val newList = deltaBuffered.clearDeltas().applyDelta(delta)

      val oldIDs = deltaBuffered.toList.toSet
      val newIDs = newList.toList.toSet

      val removed = oldIDs -- newIDs
      removed.foreach { _.task.disconnect() }

      newList
    }

}
