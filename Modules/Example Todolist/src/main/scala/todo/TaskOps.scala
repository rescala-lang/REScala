package todo

import kofre.datatypes.ReplicatedList
import kofre.dotted.Dotted
import kofre.base.Uid
import kofre.syntax.{DeltaBuffer, ReplicaId}
import rescala.default.*

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.nowarn

class TaskOps(taskRefs: TaskReferences, replicaID: Uid) {

  type State = DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]

  given ReplicaId = replicaID

  def handleCreateTodo(createTodo: Event[String]): Fold.Branch[State] = createTodo.act { desc =>
    val taskid = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"
    TaskReferences.lookupOrCreateTaskRef(taskid, Some(TaskData(desc)))
    val taskref = TaskRef(taskid)
    current.clearDeltas().prepend(taskref)
  }

  def handleRemoveAll(removeAll: Event[Any]): Fold.Branch[State] = removeAll.act { _ =>
    current.clearDeltas().deleteBy { (taskref: TaskRef) =>
      val isDone = taskref.task.value.read.exists(_.done)
      // todo, move to observer, disconnect during transaction does not respect rollbacks
      if (isDone) taskref.task.disconnect()
      isDone
    }
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
