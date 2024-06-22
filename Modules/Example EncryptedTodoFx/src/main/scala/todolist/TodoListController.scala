package todolist

import javafx.collections.{FXCollections, ObservableList}
import rdts.syntax.LocalUid
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object TodoListController {
  val replicaId: LocalUid = LocalUid.gen()

  private val crdt: SyncedTodoListCrdt = new SyncedTodoListCrdt(replicaId)

  def handleUpdated(before: Map[UUID, TodoEntry], after: Map[UUID, TodoEntry]): Unit = {
    Platform.runLater {
      val added   = after.keySet.diff(before.keySet)
      val removed = before.keySet.diff(after.keySet)
      val changed = (before.keySet -- removed)
        .map { uuid => uuid -> (before(uuid), after(uuid)) }
        .filter { case (uuid, (b, a)) => b != a }
        .map { case (uuid, (b, a)) => uuid -> a }

      uuidToTodoEntryProperties.addAll(added.map(uuid => uuid -> ObjectProperty(after(uuid))))
      observableUuidList.addAll(added.asJava)

      changed.foreach { case (k, v) => uuidToTodoEntryProperties(k).set(v) }

      observableUuidList.removeAll(removed.asJava)
      removed.foreach { uuid =>
        uuidToTodoEntryProperties.remove(uuid)
      }

    }
  }

  val observableUuidList: ObservableList[UUID] =
    FXCollections.observableList(new java.util.ArrayList[UUID](crdt.values.keys.asJavaCollection))

  private val uuidToTodoEntryProperties: mutable.Map[UUID, ObjectProperty[TodoEntry]] =
    new ConcurrentHashMap[UUID, ObjectProperty[TodoEntry]]().asScala

  def getTodo(uuid: UUID): Option[ObjectProperty[TodoEntry]] =
    uuidToTodoEntryProperties.get(uuid)

  def addTodo(todoEntry: TodoEntry): Unit = {
    val uuid = UUID.randomUUID()
    crdt.put(uuid, todoEntry)
    uuidToTodoEntryProperties.put(uuid, ObjectProperty[TodoEntry](todoEntry))
    observableUuidList.add(uuid)
    ()
  }

  def removeTodo(uuid: UUID): Unit = {
    crdt.remove(uuid)
    observableUuidList.remove(uuid)
    uuidToTodoEntryProperties.remove(uuid)
    ()
  }

  def changeTodo(uuid: UUID, changedEntry: TodoEntry): Unit = {
    if uuid == null || changedEntry == null then {
      println("uuid null for " + changedEntry)
    } else {
      val oldTodo = crdt.get(uuid)
      if oldTodo.contains(changedEntry) then return

      println(s"$uuid -> $changedEntry changed from $oldTodo")
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

  def remoteAddresses: Set[String] = crdt.remoteAddresses
}
