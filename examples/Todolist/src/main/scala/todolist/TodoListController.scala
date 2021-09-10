package de.ckuessner
package todolist

import com.typesafe.scalalogging.Logger
import javafx.collections.{FXCollections, ObservableList}
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty

import java.util.UUID
import scala.jdk.CollectionConverters._

object TodoListController {
  val replicaId: String = UUID.randomUUID().toString.substring(0, 4)
  private val crdt: SyncedTodoListCrdt = new SyncedTodoListCrdt(replicaId)

  private val LOG = Logger(getClass)

  def handleUpdated(before: Map[UUID, TodoEntry], after: Map[UUID, TodoEntry]): Unit = {
    val added = after.keySet.diff(before.keySet)
    val removed = before.keySet.diff(after.keySet)
    val changed = (before.keySet -- removed)
      .map { key => key -> (before(key), after(key)) }
      .filter { case (key, (b, a)) => b != a }
      .map { case (uuid, (b, a)) => uuid -> a }

    Platform.runLater {
      uuidToTodoEntryProperties = uuidToTodoEntryProperties ++ added.map(uuid => uuid -> ObjectProperty(after(uuid)))

      changed.foreach { case (k, v) => uuidToTodoEntryProperties(k).set(v) }

      observableUuidList.addAll(added.asJavaCollection)

      observableUuidList.removeAll(removed.asJavaCollection)
    }
  }

  val observableUuidList: ObservableList[UUID] =
    FXCollections.observableList(new java.util.ArrayList[UUID](crdt.values.keys.asJavaCollection))

  private var uuidToTodoEntryProperties: Map[UUID, ObjectProperty[TodoEntry]] =
    Map[UUID, ObjectProperty[TodoEntry]]()

  def getTodo(uuid: UUID): Option[ObjectProperty[TodoEntry]] =
    uuidToTodoEntryProperties.get(uuid)

  def addTodo(todoEntry: TodoEntry): Unit = {
    val uuid = UUID.randomUUID()
    crdt.put(uuid, todoEntry)
    uuidToTodoEntryProperties = uuidToTodoEntryProperties + (uuid -> ObjectProperty[TodoEntry](todoEntry))
    observableUuidList.add(uuid)
  }

  def removeTodo(uuid: UUID): Unit = {
    crdt.remove(uuid)
    observableUuidList.remove(uuid)
  }

  def changeTodo(uuid: UUID, changedEntry: TodoEntry): Unit = {
    if (!crdt.get(uuid).contains(changedEntry)) {
      LOG.debug(s"$uuid -> $changedEntry changed from ${crdt.get(uuid)}")
      uuidToTodoEntryProperties(uuid).set(changedEntry)
      crdt.put(uuid, changedEntry)
    }
  }

  def stop(): Unit = {
    crdt.shutdown()
  }

  def connectionString: String = s"${crdt.replicaId}@${crdt.address}"

  def connect(connectionString: String): Unit = crdt.connect(connectionString)

  def todos: Map[UUID, TodoEntry] = crdt.values

  def peers: Map[String, String] = crdt.peers

}