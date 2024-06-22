package benchmarks.encrdt.todolist

import java.util.UUID

sealed trait ToDoListInteraction

case class AddToDoItem(uuid: UUID, toDoEntry: ToDoEntry) extends ToDoListInteraction

case class CompleteToDoItem(uuid: UUID) extends ToDoListInteraction

//case class ChangeToDoItemText(uuid: UUID, newText: String) extends ToDoListInteraction

case class RemoveToDoItems(uuids: Seq[UUID]) extends ToDoListInteraction
