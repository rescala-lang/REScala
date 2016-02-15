package benchmarks.chatserver

import rescala.reactives.Events
import rescala.graph.Spores
import rescala.propagation.Turn
import rescala.engines.Engine

import scala.collection.LinearSeq
import scala.collection.immutable.Queue

class ChatServer[S <: Spores]()(implicit val engine: Engine[S, Turn[S]]) {

  import engine._

  type Room = Int
  type Client = Event[String]
  type Clients = Var[List[Client]]
  type History = Signal[LinearSeq[String]]

  val rooms = new java.util.concurrent.ConcurrentHashMap[Room, Clients]()
  val histories = new java.util.concurrent.ConcurrentHashMap[Room, History]()

  def join(client: Client, room: Room) = {
    rooms.get(room).transform(clients => client :: clients)
  }

  def create(room: Room): Boolean = {
    val clients: Clients = Var(Nil)
    val history: History = Events.dynamic(clients) { implicit t =>
      val messages: List[String] = clients(t).flatMap(_.apply(t))
      if (messages.isEmpty) None else Some(messages)
    }.fold(Queue[String]()) { (queue, v) =>
      if (queue.length >= 100) queue.tail.enqueue(v) else queue.enqueue(v)
    }

    rooms.putIfAbsent(room, clients) == null &&
      histories.put(room, history) == null
  }

}
