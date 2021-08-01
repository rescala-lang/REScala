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

  case class State(
      list: RGA[String, DietMapCContext],
      signalMap: Map[String, Signal[LWWRegister[TodoTask, DietMapCContext]]],
      uiMap: Map[String, TypedTag[LI]],
      evtMap: Map[String, Event[String]]
  )

  val myID = ThreadLocalRandom.current().nextLong().toHexString

  def listInitial = RGA[String, DietMapCContext](myID)

  def handleCreateTodo(s: => State)(desc: String): State = s match {
    case State(list, signalMap, uiMap, evtMap) =>
      val newTask = TodoTask(desc = desc)

      val (signal, ui, evt) = TodoTaskView.signalAndUI(myID, newTask.id, Some(newTask), toggleAll)

      val newList = list.resetDeltaBuffer().prepend(newTask.id)

      State(newList, signalMap + (newTask.id -> signal), uiMap + (newTask.id -> ui), evtMap + (newTask.id -> evt))
  }

  def handleRemoveAll(s: => State): State = s match {
    case State(list, signalMap, uiMap, evtMap) =>
      val removeIDs = signalMap.values.collect { sig =>
        sig.readValueOnce.read match {
          case Some(TodoTask(id, _, true)) => id
        }
      }.toSet

      val newList = list.resetDeltaBuffer().deleteBy(removeIDs.contains)

      removeIDs.foreach { signalMap(_).disconnect() }

      State(newList, signalMap -- removeIDs, uiMap -- removeIDs, evtMap -- removeIDs)
  }

  def handleRemove(s: => State)(id: String): State = s match {
    case State(list, signalMap, uiMap, evtMap) =>
      val newList = list.resetDeltaBuffer().deleteBy(_ == id)
      signalMap(id).disconnect()

      State(newList, signalMap - id, uiMap - id, evtMap - id)
  }

  def handleDelta(s: => State)(delta: Delta[RGA.State[String, DietMapCContext]]): State = s match {
    case State(list, signalMap, uiMap, evtMap) =>
      val newList = list.resetDeltaBuffer().applyDelta(delta)

      val oldIDs = list.toList.toSet
      val newIDs = newList.toList.toSet

      val added   = (newIDs -- oldIDs).toList
      val removed = oldIDs -- newIDs

      val (addedSignals, addedUIs, addedEvts) = added.map { id =>
        TodoTaskView.signalAndUI(myID, id, None, toggleAll)
      }.unzip3

      removed.foreach { signalMap(_).disconnect() }

      val newSignalMap = signalMap -- removed ++ (added zip addedSignals)
      val newUIMap     = uiMap -- removed ++ (added zip addedUIs)
      val newEvtMap    = evtMap -- removed ++ (added zip addedEvts)

      State(newList, newSignalMap, newUIMap, newEvtMap)
  }

}
