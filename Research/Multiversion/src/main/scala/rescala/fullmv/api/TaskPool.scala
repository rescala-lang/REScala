package rescala.fullmv.api

import scala.language.existentials

sealed abstract class Task
case class NoChangeNotification(txn: Transaction, node: SignalVersionList[_]) extends Task {
  def apply(): Unit = node.notifyUnchanged(txn)
}
case class ChangeNotification(txn: Transaction, node: SignalVersionList[_]) extends Task {
  def apply(): Unit = node.notifyChanged(txn)
}
case class Framing(txn: Transaction, node: SignalVersionList[_]) extends Task {
  def apply(): Unit = {
    val outgoings = node.incrementFrame(txn)
  }
}

trait TaskPool {
  // highest priority, as these operations can never suspend and reduce suspensions on other reevaluations
  val noChangeNotifications: TaskList[NoChangeNotification]
  val changeNotifications: TaskList[Task]
  val framings: TaskList[Framing]

  def addFraming(txn: Transaction, node: SignalVersionList[_]): Unit = {
    framings.enqueue(Framing(txn, node))
  }
  def addChangeNotification(txn: Transaction, node: SignalVersionList[_]): Unit = {
    changeNotifications.enqueue(ChangeNotification(txn, node))
  }
  def addNoChangeNotification(txn: Transaction, node: SignalVersionList[_]): Unit = {
    noChangeNotifications.enqueue(NoChangeNotification(txn, node))
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
