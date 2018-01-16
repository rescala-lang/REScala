package benchmarks.chatserver

import rescala.core.{Scheduler, Struct}

import scala.collection.LinearSeq
import scala.collection.immutable.Queue

class ChatServer[S <: Struct]()(val engine: Scheduler[S]) {

  import engine._

  type Room = Int
  type Client = Event[String]
  type Clients = Var[List[Client]]
  type NewMessages = Event[List[String]]
  type History = Signal[LinearSeq[String]]

  val rooms = new java.util.concurrent.ConcurrentHashMap[Room, Clients]()
  val histories = new java.util.concurrent.ConcurrentHashMap[Room, History]()

  def join(client: Client, room: Room) = {
    rooms.get(room).transform(clients => client :: clients)
  }

  def create(room: Room): Boolean = {
    val clients: Clients = Var(Nil)
    val newMessages: NewMessages = Event.dynamic {
      val messages: List[String] = clients().flatMap(_.apply())
      if (messages.isEmpty) None else Some(messages)
    }
    val history: History = newMessages.fold(Queue[String]()) { (queue, v) =>
      if (queue.length >= 100) queue.tail.enqueue(v) else queue.enqueue(v)
    }

    rooms.putIfAbsent(room, clients) == null &&
      histories.put(room, history) == null
  }

}
