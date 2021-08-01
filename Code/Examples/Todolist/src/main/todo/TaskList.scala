package src.main.todo

import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.LI
import rescala.default._
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.Delta
import rescala.extra.lattices.delta.crdt.reactive.RGA._
import rescala.extra.lattices.delta.crdt.reactive.{LWWRegister, RGA}
import scalatags.JsDom.TypedTag

import java.util.concurrent.ThreadLocalRandom

class TaskList(toggleAll: Event[UIEvent]) {

  type State = RGA[TaskRef, DietMapCContext]

  val replicaId: String = ThreadLocalRandom.current().nextLong().toHexString

  def listInitial: State = RGA[TaskRef, DietMapCContext](replicaId)

  def handleCreateTodo(state: => State)(desc: String): State = {

    val newTask = TaskData(desc)

    val taskref = TaskRef.signalAndUI(replicaId, Some(newTask), toggleAll)

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
