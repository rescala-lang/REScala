package todo

import org.scalajs.dom.UIEvent
import rescala.default._
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.Delta
import rescala.extra.lattices.delta.crdt.reactive.RGA
import rescala.extra.lattices.delta.crdt.reactive.RGA._
import todo.Todolist.replicaId

import java.util.concurrent.ThreadLocalRandom

class TaskList(toggleAll: Event[UIEvent], taskRefs: TaskRefObj) {

  type State = RGA[TaskRef, DietMapCContext]

  def listInitial: State = RGA[TaskRef, DietMapCContext](replicaId)

  def handleCreateTodo(state: => State)(desc: String): State = {
    val taskid   = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"
    val taskref = taskRefs.lookupOrCreateTaskRef(taskid, Some(TaskData(desc)))
    state.resetDeltaBuffer().prepend(taskref)
  }

  def handleRemoveAll(state: => State, dt: DynamicTicket): State = {
    state.resetDeltaBuffer().deleteBy { taskref =>
      val isDone = dt.depend(taskref.task).read.exists(_.done)
      // todo, move to observer, disconnect during transaction does not respect rollbacks
      if (isDone) taskref.task.disconnect()
      isDone
    }
  }

  def handleRemove(state: => State)(id: String): State = {
    state.resetDeltaBuffer().deleteBy { taskref =>
      val delete = taskref.id == id
      // todo, move to observer, disconnect during transaction does not respect rollbacks
      if (delete) taskref.task.disconnect()
      delete
    }
  }

  def handleDelta(s: => State)(delta: Delta[RGA.State[TaskRef, DietMapCContext]]): State = {
    val list = s

    val newList = list.resetDeltaBuffer().applyDelta(delta)

    val oldIDs = list.toList.toSet
    val newIDs = newList.toList.toSet

    val removed = oldIDs -- newIDs
    removed.foreach { _.task.disconnect() }

    newList
  }

}
