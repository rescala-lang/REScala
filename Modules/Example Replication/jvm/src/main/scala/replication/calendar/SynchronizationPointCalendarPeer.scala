package replication.calendar

import rdts.base.{Lattice, Uid}
import rdts.datatypes.contextual.ReplicatedSet
import rdts.dotted.{Dotted, DottedLattice}
import replication.calendar.SyncMessage.*

import scala.io.StdIn.readLine
import scala.util.matching.Regex

class SynchronizationPointCalendarPeer(id: Uid, listenPort: Int, connectTo: List[(String, Int)]) {

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  val add: Regex    = """add (\w+) (\d+) (\d+)""".r
  val remove: Regex = """remove (\w+) (\d+) (\d+)""".r
  val change: Regex = """change (\w+) (\d+) (\d+) (\d+) (\d+)""".r

  val calendar = new CalendarProgram(id, synchronizationPoint)

  @volatile var tokens: RaftTokens = RaftTokens.init(id)

  var callbacks: List[(String, () => Unit)] = Nil

  def synchronizationPoint(value: String)(callback: => Unit): Unit = {
    tokens = tokens.acquire(value)
    callbacks ::= Tuple2(value, () => callback)
  }

  def checkCallbacks(): Unit = {
    val res  = callbacks.groupBy { case (t, c) => tokens.isOwned(t) }
    val mine = res.getOrElse(true, Nil)
    callbacks = res.getOrElse(false, Nil)
    tokens = mine.map(_._1).sorted.distinct.foldLeft(tokens) { case (s, tok) => tokens.free(tok) }
    mine.map(_._2).foreach(_.apply())
  }

  def sendDeltas(): Unit = {

    calendar.replicated.foreach { case (id, set) =>
      set.now.deltaBuffer.reduceOption(Lattice[CalendarState].merge).foreach(delta => delta)
    }

    tokens.want.deltaBuffer.reduceOption(Lattice[Dotted[ReplicatedSet[Token]]].merge).foreach { state =>
      WantMessage(state)
    }

    tokens.tokenFreed.deltaBuffer.reduceOption(DottedLattice[ReplicatedSet[Token]].merge).foreach { state =>
      FreeMessage(state)
    }

    val _ = RaftMessage(tokens.tokenAgreement)

    // calendar.replicated.foreach { case (id, r) => r.transform(_.resetDeltaBuffer()) }
  }

  def splitState(
      atoms: Iterable[CalendarState],
      merged: CalendarState
  ): (Iterable[CalendarState], Iterable[CalendarState]) = {
    val a =
      if atoms.isEmpty then Lattice[CalendarState].decompose(merged)
      else atoms

    val atomsSize = a.size

    (a.take(atomsSize / 2), a.drop(atomsSize / 2))
  }

  def printStatus() = {
    println(s"> Cal :\n  work: ${calendar.work.now.data.elements}\n  vaca: ${calendar.vacation.now.data.elements}")
    println(
      s"> Raft: ${tokens.tokenAgreement.leader} (${tokens.tokenAgreement.currentTerm})\n  ${tokens.tokenAgreement.values}"
    )
    println(s"> Want: ${tokens.want.data.elements}")
    println(s"> Free: ${tokens.tokenFreed.data.elements}")
    println("")
  }

  val globalLock = new Object()

  def run(): Unit = {
    val _ = { (message: SyncMessage) =>
      globalLock.synchronized {
        println(s"> Recv: $message")

        message match {
          case AppointmentMessage(deltaState, id) =>
            val set = calendar.replicated(id)
            set.transform(_.applyDelta(deltaState))
          case WantMessage(state) =>
            tokens = tokens.applyWant(state)
          case FreeMessage(state) =>
            tokens = tokens.applyFree(state)
          case RaftMessage(state) => {
            tokens = tokens.applyRaft(state)
          }
        }
        printStatus()

      }

    }

    while true do {
      val line = readLine()
      globalLock.synchronized {
        line match {
          case add(c, start, end) =>
            val cal         = if c == "work" then calendar.work else calendar.vacation
            val appointment = Appointment(start.toInt, end.toInt)
            calendar.add_appointment(cal, appointment)

          case remove(c, start, end) =>
            val cal         = if c == "work" then calendar.work else calendar.vacation
            val appointment = Appointment(start.toInt, end.toInt)
            calendar.remove_appointment(cal, appointment)

          case change(c, start, end, nstart, nend) =>
            val cal         = if c == "work" then calendar.work else calendar.vacation
            val appointment = Appointment(start.toInt, end.toInt)
            calendar.change_time(cal, appointment, nstart.toInt, nend.toInt)

          case "elements" =>
            println(calendar.work.now.data.elements)
            println(calendar.vacation.now.data.elements)

          case "lead" =>
            tokens = tokens.lead()

          case "exit" =>
            System.exit(0)

          case _ =>
            println("doing housekeeping")
            tokens = tokens.update()
            checkCallbacks()
        }
        printStatus()

      }

      sendDeltas()

    }

  }
}
