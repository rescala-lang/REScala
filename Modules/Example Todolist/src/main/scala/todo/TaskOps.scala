package todo

import rescala.default.*

import java.util.concurrent.ThreadLocalRandom
import scala.annotation.unused

class TaskOps(@unused taskrefs: TaskReferences) {

  type State = List[TaskRef]

  def handleCreateTodo(createTodo: Event[String]): Fold.Branch[State] = createTodo.act { desc =>
    val taskid = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"
    TaskReferences.lookupOrCreateTaskRef(taskid, Some(TaskData(desc)))
    val taskref = TaskRef(taskid)
    taskref :: current
  }

  def handleRemoveAll(removeAll: Event[Any]): Fold.Branch[State] = removeAll.act { _ =>
    current.filterNot { (taskref: TaskRef) =>
      val isDone = taskref.task.value.exists(_.done)
      // todo, move to observer, disconnect during transaction does not respect rollbacks
      if (isDone) taskref.task.disconnect()
      isDone
    }
  }

  def handleRemove(state: State)(id: String): State = {
    state.filterNot { (taskref: TaskRef) =>
      val delete = taskref.id == id
      // todo, move to observer, disconnect during transaction does not respect rollbacks
      if (delete) taskref.task.disconnect()
      delete
    }
  }

}
