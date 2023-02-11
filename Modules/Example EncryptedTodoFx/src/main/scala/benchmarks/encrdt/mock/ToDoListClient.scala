package benchmarks.encrdt.mock

import benchmarks.encrdt.todolist.ToDoEntry

import java.util.UUID

trait ToDoListClient {
  def completeToDoItem(uuid: UUID): Unit
  def addToDoItem(uuid: UUID, toDoEntry: ToDoEntry): Unit
  def removeToDoItems(uuids: Seq[UUID]): Unit

  def disseminationStats: DisseminationStats
}
