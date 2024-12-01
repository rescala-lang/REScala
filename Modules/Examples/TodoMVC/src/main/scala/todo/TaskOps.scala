package todo

import rdts.base.{LocalUid, Uid}
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import reactives.default.*
import todo.TodoDataManager.TodoRepState

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.unused

object TaskOps {
  def resetBuffer[T] = Fold.Branch[DeltaBuffer[T]](Nil, isStatic = false, _ => Fold.current.clearDeltas())
}

// `taskrefs` is unused as a reference, but is used indirectly so this parameter serves as a requirement
// that a `taskrefs` needs to be created before taskops may be used
class TaskOps(@unused taskrefs: TaskReferences, replicaID: LocalUid) {

  type State = DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]

  given LocalUid = replicaID

  def handleCreateTodo(createTodo: Event[String]): Fold.Branch[State] = createTodo.branch { desc =>
    val taskid = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"
    TaskReferences.lookupOrCreateTaskRef(taskid, Some(TaskData(desc)))
    val taskref = TaskRef(taskid)
    current.modd(_.prepend(taskref))
  }

  def handleRemoveAll(removeAll: Event[Any]): Fold.Branch[State] =
    removeAll.branch: _ =>
      current.modd(_.deleteBy { (taskref: TaskRef) =>
        val isDone = taskref.task.value.state.read.exists(_.done)
        // todo, move to observer, disconnect during transaction does not respect rollbacks
        if isDone then taskref.task.disconnect()
        isDone
      })

  def handleRemove(state: State)(id: String): State = {
    state.modd(_.deleteBy { (taskref: TaskRef) =>
      val delete = taskref.id == id
      // todo, move to observer, disconnect during transaction does not respect rollbacks
      if delete then taskref.task.disconnect()
      delete
    })
  }

  def handleDelta(deltaEvent: Event[TodoDataManager.TodoRepState]): Fold.Branch[State] =
    deltaEvent.branch { allDeltas =>
      val deltaBuffered = current

      val delta = (allDeltas: TodoRepState).list

      val newList = deltaBuffered.applyDelta(delta)

      val oldIDs = deltaBuffered.data.toList.toSet
      val newIDs = newList.data.toList.toSet

      val removed = oldIDs -- newIDs
      removed.foreach { _.task.disconnect() }

      newList
    }

}
