package rescala.fullmv.api

import scala.language.existentials

sealed abstract class Task
case class NoChangeNotification(node: SignalVersionList[_], txn: Transaction, maybeFollowFrame: Option[Transaction]) extends Task {
  def apply(): Unit = node.notify(txn, false, maybeFollowFrame)
}
case class ChangeNotification(node: SignalVersionList[_], txn: Transaction, maybeFollowFrame: Option[Transaction]) extends Task {
  def apply(): Unit = node.notify(txn, true, maybeFollowFrame)
}
case class Framing(node: SignalVersionList[_], txn: Transaction) extends Task {
  def apply(): Unit = node.incrementFrame(txn)
}
case class SupersedingFraming(node: SignalVersionList[_], txn: Transaction, superseded: Transaction) extends Task {
  def apply(): Unit = node.incrementSupersedeFrame(txn, superseded)
}

trait TaskPool {
  // highest priority, as these operations can never suspend and reduce suspensions on other reevaluations
  val noChangeNotifications: TaskList[NoChangeNotification]
  val changeNotifications: TaskList[Task]
  val framings: TaskList[Framing]
  val supersedingFramings: TaskList[SupersedingFraming]

  def addFraming(node: SignalVersionList[_], txn: Transaction): Unit = {
    framings.enqueue(Framing(node, txn))
  }
  def addSupersedingFraming(node: SignalVersionList[_], txn: Transaction, superseded: Transaction): Unit = {
    supersedingFramings.enqueue(SupersedingFraming(node, txn, superseded))
  }
  def addChangeNotification(node: SignalVersionList[_], txn: Transaction, maybeFollowFrame: Option[Transaction]): Unit = {
    changeNotifications.enqueue(ChangeNotification(node, txn, maybeFollowFrame))
  }
  def addNoChangeNotification(node: SignalVersionList[_], txn: Transaction, maybeFollowFrame: Option[Transaction]): Unit = {
    noChangeNotifications.enqueue(NoChangeNotification(node, txn, maybeFollowFrame))
  }

  def dequeue(): Option[Task] = {
    noChangeNotifications.dequeue().orElse {
      changeNotifications.dequeue().orElse {
        framings.dequeue()
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
