package rescala.fullmv

import rescala.graph.Reactive

sealed abstract class Task
case class Notification(node: Reactive[FullMVStruct], txn: FullMVTurn, changed: Boolean, maybeFollowFrame: Option[FullMVTurn]) extends Task {
  def apply(): Unit = txn.notify(node, changed, maybeFollowFrame)
}
case class IncrementFrame(node: Reactive[FullMVStruct], txn: FullMVTurn) extends Task {
  def apply(): Unit = txn.incrementFrame(node)
}
case class IncrementSupersedeFrame(node: Reactive[FullMVStruct], txn: FullMVTurn, superseded: FullMVTurn) extends Task {
  def apply(): Unit = txn.incrementSupersedeFrame(node, superseded)
}

trait TaskPool {
  def addFraming(node: Reactive[FullMVStruct], txn: FullMVTurn): Unit
  def addSupersedingFraming(node: Reactive[FullMVStruct], txn: FullMVTurn, superseded: FullMVTurn): Unit
  def addNotification(node: Reactive[FullMVStruct], txn: FullMVTurn, changed: Boolean, maybeFollowFrame: Option[FullMVTurn]): Unit
}

class StoringTaskPool extends TaskPool {
  val supersedingFramings: TaskList[IncrementSupersedeFrame] = new TaskList[IncrementSupersedeFrame]
  val framings: TaskList[IncrementFrame] = new TaskList[IncrementFrame]
  val notifications: TaskList[Notification] = new TaskList[Notification]

  override def addFraming(node: Reactive[FullMVStruct], txn: FullMVTurn): Unit = {
    framings.enqueue(IncrementFrame(node, txn))
  }
  override def addSupersedingFraming(node: Reactive[FullMVStruct], txn: FullMVTurn, superseded: FullMVTurn): Unit = {
    supersedingFramings.enqueue(IncrementSupersedeFrame(node, txn, superseded))
  }
  override def addNotification(node: Reactive[FullMVStruct], txn: FullMVTurn, changed: Boolean, maybeFollowFrame: Option[FullMVTurn]): Unit = {
    notifications.enqueue(Notification(node, txn, changed, maybeFollowFrame))
  }

  def dequeue(): Option[Task] = {
    // dequeue framings before notifications to increase the chances of not having to perform complete framings
    // dequeue superseding framings very first because they may stop framings from branching further
    supersedingFramings.dequeue().orElse {
      framings.dequeue().orElse {
        notifications.dequeue()
      }
    }
  }
}

class TaskListEntry[T](val task: T, var next: Option[TaskListEntry[T]] = None)

class TaskList[T] {
  var head: Option[TaskListEntry[T]] = None
  var tail: Option[TaskListEntry[T]] = None
  def enqueue(task: T): Unit = {
    val entry = Some(new TaskListEntry[T](task))
    tail match {
      case None =>
        head = entry
        tail = entry
      case Some(other) =>
        other.next = entry
        tail = entry
    }
  }

  def dequeue(): Option[T] = {
    val result = head.map(_.task)
    if(head.isDefined) {
        head = head.get.next
        if(head.isEmpty) {
          tail = None
        }
      }
    result
  }
}
