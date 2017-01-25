package rescala.fullmv.api

import scala.annotation.tailrec


sealed abstract class Task extends Function0[Unit]
case class NoChangeNotification(txn: Transaction, node: SignalVersionList[_]) extends Task {
  override def apply(): Unit = node.notifyUnchanged(txn)
}
case class ChangeNotification(txn: Transaction, node: SignalVersionList[_]) extends Task {
  override def apply(): Unit = node.notifyChanged(txn)
}
case class Framing(txn: Transaction, node: SignalVersionList[_]) extends Task {
  override def apply(): Unit = {
    val outgoings = node.incrementFrame(txn)
  }
}

trait TaskPool {
  // highest priority, as these operations can never suspend and reduce suspensions on other reevaluations
  val noChangeNotifications: TaskList[NoChangeNotification]
  val changeNotifications: TaskList[Task]
  val framings: TaskList[Framing]

  def addFraming(txn: Transaction, node: SignalVersionList[_]): Unit = {
    framings.enqueue(new Framing(txn, node))
  }
  def addChangeNotification(txn: Transaction, node: SignalVersionList[_]): Unit = {
    changeNotifications.enqueue(new ChangeNotification(txn, node))
  }
  def addNoChangeNotification(txn: Transaction, node: SignalVersionList[_]): Unit = {
    noChangeNotifications.enqueue(new NoChangeNotification(txn, node))
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
    val result = head.map(_.task);
    if(head.isDefined) {
        head = head.get.next
        if(head == None) {
          tail = None
        }
      }
    result
  }
}
