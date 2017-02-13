package rescala.fullmv.api

import scala.language.existentials

sealed abstract class Task
case class Notification(node: SignalVersionList[_], txn: Transaction, changed: Boolean, maybeFollowFrame: Option[Transaction]) extends Task {
  def apply(): Unit = node.notify(txn, changed, maybeFollowFrame)
}
case class IncrementFrame(node: SignalVersionList[_], txn: Transaction) extends Task {
  def apply(): Unit = node.incrementFrame(txn)
}
case class IncrementSupersedeFrame(node: SignalVersionList[_], txn: Transaction, superseded: Transaction) extends Task {
  def apply(): Unit = node.incrementSupersedeFrame(txn, superseded)
}

trait TaskPool {
  def addFraming(node: SignalVersionList[_], txn: Transaction): Unit
  def addSupersedingFraming(node: SignalVersionList[_], txn: Transaction, superseded: Transaction): Unit
  def addNotification(node: SignalVersionList[_], txn: Transaction, changed: Boolean, maybeFollowFrame: Option[Transaction]): Unit
}

class StoringTaskPool extends TaskPool {
  val supersedingFramings: TaskList[IncrementSupersedeFrame] = new TaskList[IncrementSupersedeFrame]
  val framings: TaskList[IncrementFrame] = new TaskList[IncrementFrame]
  val notifications: TaskList[Notification] = new TaskList[Notification]

  override def addFraming(node: SignalVersionList[_], txn: Transaction): Unit = {
    framings.enqueue(IncrementFrame(node, txn))
  }
  override def addSupersedingFraming(node: SignalVersionList[_], txn: Transaction, superseded: Transaction): Unit = {
    supersedingFramings.enqueue(IncrementSupersedeFrame(node, txn, superseded))
  }
  override def addNotification(node: SignalVersionList[_], txn: Transaction, changed: Boolean, maybeFollowFrame: Option[Transaction]): Unit = {
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
