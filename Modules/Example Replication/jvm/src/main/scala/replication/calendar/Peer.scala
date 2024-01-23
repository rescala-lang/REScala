package replication.calendar

import Bindings._
import SyncMessage.{AppointmentMessage, CalendarState, FreeMessage, RaftMessage, WantMessage}
import kofre.base.{Lattice, Uid}
import kofre.datatypes.contextual.ReplicatedSet
import kofre.dotted.{DottedLattice, Dotted}
import loci.communicator.tcp.TCP
import loci.registry.Registry
import loci.transmitter.{RemoteAccessException, RemoteRef}

import java.util.concurrent._
import scala.concurrent.Future
import scala.io.StdIn.readLine
import scala.util.matching.Regex
import scala.util.{Failure, Success}

class Peer(id: Uid, listenPort: Int, connectTo: List[(String, Int)]) {

  val registry = new Registry

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  val add: Regex    = """add (\w+) (\d+) (\d+)""".r
  val remove: Regex = """remove (\w+) (\d+) (\d+)""".r
  val change: Regex = """change (\w+) (\d+) (\d+) (\d+) (\d+)""".r

  val calendar = new CalendarProgram(id, synchronizationPoint)

  var remoteToAddress: Map[RemoteRef, (String, Int)] = Map()

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

  def connectToRemote(address: (String, Int)): Unit = address match {
    case (ip, port) =>
      new FutureTask[Unit](() => {
        def attemptReconnect(): Unit = {
          registry.connect(TCP(ip, port)).onComplete {
            case Success(value) =>
              remoteToAddress = remoteToAddress.updated(value, (ip, port))
            case Failure(_) =>
              Thread.sleep(1000)
              attemptReconnect()
          }
        }

        attemptReconnect()
      }).run()
  }

  def sendDeltas(): Unit = {
    registry.remotes.foreach { rr =>
      val remoteReceiveSyncMessage = registry.lookup(receiveSyncMessageBinding, rr)

      calendar.replicated.foreach { case (id, set) =>
        set.now.deltaBuffer.reduceOption(Lattice[CalendarState].merge).foreach(sendRecursive(
          remoteReceiveSyncMessage,
          _,
          id
        ))
      }

      tokens.want.deltaBuffer.reduceOption(Lattice[Dotted[ReplicatedSet[Token]]].merge).foreach { state =>
        remoteReceiveSyncMessage(WantMessage(state))
      }

      tokens.tokenFreed.deltaBuffer.reduceOption(DottedLattice[ReplicatedSet[Token]].merge).foreach { state =>
        remoteReceiveSyncMessage(FreeMessage(state))
      }

      remoteReceiveSyncMessage(RaftMessage(tokens.tokenAgreement))

    // calendar.replicated.foreach { case (id, r) => r.transform(_.resetDeltaBuffer()) }
    }
  }

  def splitState(
      atoms: Iterable[CalendarState],
      merged: CalendarState
  ): (Iterable[CalendarState], Iterable[CalendarState]) = {
    val a =
      if (atoms.isEmpty) Lattice[CalendarState].decompose(merged)
      else atoms

    val atomsSize = a.size

    (a.take(atomsSize / 2), a.drop(atomsSize / 2))
  }

  def sendRecursive(
      remoteReceiveSyncMessage: SyncMessage => Future[Unit],
      delta: Dotted[ReplicatedSet[Appointment]],
      crdtid: String,
  ): Unit = new FutureTask[Unit](() => {
    def attemptSend(atoms: Iterable[CalendarState], merged: CalendarState): Unit = {
      remoteReceiveSyncMessage(AppointmentMessage(merged, crdtid)).failed.foreach {
        case e: RemoteAccessException => e.reason match {
            case RemoteAccessException.RemoteException(name, _) if name.contains("JsonReaderException") =>
              val (firstHalf, secondHalf) = splitState(atoms, merged)

              attemptSend(firstHalf, firstHalf.reduce(Lattice[CalendarState].merge))
              attemptSend(secondHalf, secondHalf.reduce(Lattice[CalendarState].merge))
            case _ => e.printStackTrace()
          }

        case e => e.printStackTrace()
      }
    }

    attemptSend(List(), delta)
  }).run()

  def printStatus() = {
    println(s"> Cal :\n  work: ${calendar.work.now.elements}\n  vaca: ${calendar.vacation.now.elements}")
    println(
      s"> Raft: ${tokens.tokenAgreement.leader} (${tokens.tokenAgreement.currentTerm})\n  ${tokens.tokenAgreement.values}"
    )
    println(s"> Want: ${tokens.want.elements}")
    println(s"> Free: ${tokens.tokenFreed.elements}")
    println("")
  }

  val globalLock = new Object()

  def run(): Unit = {
    registry.bindSbj(receiveSyncMessageBinding) { (remoteRef: RemoteRef, message: SyncMessage) =>
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

    println(registry.listen(TCP(listenPort)))

    connectTo.foreach(connectToRemote)

    registry.remoteLeft.monitor { rr =>
      remoteToAddress.get(rr) match {
        case Some(address) =>
          remoteToAddress = remoteToAddress.removed(rr)

          connectToRemote(address)

        case None =>
      }
    }

    while (true) {
      val line = readLine()
      globalLock.synchronized {
        line match {
          case add(c, start, end) =>
            val cal         = if (c == "work") calendar.work else calendar.vacation
            val appointment = Appointment(start.toInt, end.toInt)
            calendar.add_appointment(cal, appointment)

          case remove(c, start, end) =>
            val cal         = if (c == "work") calendar.work else calendar.vacation
            val appointment = Appointment(start.toInt, end.toInt)
            calendar.remove_appointment(cal, appointment)

          case change(c, start, end, nstart, nend) =>
            val cal         = if (c == "work") calendar.work else calendar.vacation
            val appointment = Appointment(start.toInt, end.toInt)
            calendar.change_time(cal, appointment, nstart.toInt, nend.toInt)

          case "elements" =>
            println(calendar.work.now.elements)
            println(calendar.vacation.now.elements)

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
