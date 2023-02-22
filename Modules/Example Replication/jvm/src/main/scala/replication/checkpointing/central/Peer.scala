package replication.checkpointing.central

import Bindings.*
import kofre.base.Lattice
import kofre.datatypes.AddWinsSet
import kofre.dotted.Dotted
import kofre.syntax.{DeltaBuffer, ReplicaId}
import loci.communicator.tcp.TCP
import loci.registry.Registry
import loci.transmitter.{RemoteAccessException, RemoteRef}

import java.util.concurrent.*
import scala.concurrent.Future
import scala.io.StdIn.readLine
import scala.util.matching.Regex
import scala.util.{Failure, Success}
import kofre.base.Uid

class Peer(id: Uid, listenPort: Int, connectTo: List[(String, Int)]) {

  val registry = new Registry

  given ReplicaId = id

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  val add: Regex       = """add (\d+)""".r
  val remove: Regex    = """remove (\d+)""".r
  val clear: String    = "clear"
  val elements: String = "elements"
  val size: String     = "size"
  val exit: String     = "exit"

  var set: DeltaBuffer[Dotted[AddWinsSet[Int]]] = DeltaBuffer(Dotted(AddWinsSet.empty))

  var checkpoint: Int = 0

  var changesSinceCP: SetState = Dotted(AddWinsSet.empty[Int])

  var remoteToAddress: Map[RemoteRef, (String, Int)] = Map()

  var checkpointerRR: Option[RemoteRef] = None

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
    registry.remotes.filterNot(checkpointerRR.contains).foreach { rr =>
      val remoteReceiveSyncMessage = registry.lookup(receiveSyncMessageBinding, rr)

      set.deltaBuffer.reduceOption(Lattice[SetState].merge).foreach(sendRecursive(
        remoteReceiveSyncMessage,
        _
      ))
    }

    set = set.clearDeltas()
  }

  def splitState(atoms: Iterable[SetState], merged: SetState): (Iterable[SetState], Iterable[SetState]) = {
    val a =
      if (atoms.isEmpty) Lattice[SetState].decompose(merged)
      else atoms

    val atomsSize = a.size

    (a.take(atomsSize / 2), a.drop(atomsSize / 2))
  }

  def sendRecursive(
      remoteReceiveSyncMessage: SyncMessage => Future[Unit],
      delta: Dotted[AddWinsSet[Int]]
  ): Unit = new FutureTask[Unit](() => {
    def attemptSend(atoms: Iterable[SetState], merged: SetState): Unit = {
      remoteReceiveSyncMessage(SyncMessage(checkpoint, merged)).failed.foreach {
        case e: RemoteAccessException => e.reason match {
            case RemoteAccessException.RemoteException(name, _) if name.contains("JsonReaderException") =>
              val (firstHalf, secondHalf) = splitState(atoms, merged)

              attemptSend(firstHalf, firstHalf.reduce(Lattice[SetState].merge))
              attemptSend(secondHalf, secondHalf.reduce(Lattice[SetState].merge))
            case _ => e.printStackTrace()
          }

        case e => e.printStackTrace()
      }
    }

    attemptSend(List(), delta)
  }).run()

  def assessCheckpointRecursive(): Unit =
    new FutureTask[Unit](() => {
      checkpointerRR.foreach { rr =>
        val assessCheckpoint = registry.lookup(assessCheckpointBinding, rr)

        def attemptAssess(atoms: Iterable[SetState], merged: SetState): Unit = {
          assessCheckpoint(SyncMessage(checkpoint, merged)).onComplete {
            case Failure(e: RemoteAccessException) => e.reason match {
                case RemoteAccessException.RemoteException(name, _) if name.contains("JsonReaderException") =>
                  val (firstHalf, secondHalf) = splitState(atoms, merged)

                  attemptAssess(firstHalf, firstHalf.reduce(Lattice[SetState].merge))
                  attemptAssess(secondHalf, secondHalf.reduce(Lattice[SetState].merge))
                case _ => e.printStackTrace()
              }

            case Failure(e) => e.printStackTrace()

            case Success(CheckpointMessage(cp, apply, keep)) =>
              if (cp > checkpoint) checkpoint = cp
              set = apply.foldLeft(set)((s, d) => s.applyDelta(d)).clearDeltas()
              changesSinceCP = keep
          }
        }

        attemptAssess(List(), changesSinceCP)
      }
    }).run()

  def processChangesForCheckpointing(): Unit = {
    changesSinceCP = set.deltaBuffer.foldLeft(changesSinceCP) { (acc, delta) =>
      Lattice[SetState].merge(acc, delta)
    }

    assessCheckpointRecursive()
  }

  def run(): Unit = {
    registry.bindSbj(receiveSyncMessageBinding) { (remoteRef: RemoteRef, message: SyncMessage) =>
      println(s"Received $message")

      val SyncMessage(cp, deltaState) = message

      if (checkpoint < cp) {
        assessCheckpointRecursive()
      }

      set = set.applyDelta(deltaState)

      processChangesForCheckpointing()

      sendDeltas()

      println(set.elements)
    }

    registry.bind(isCheckpointerBinding) { () => false }

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

    registry.remoteJoined.monitor { rr =>
      registry.lookup(isCheckpointerBinding, rr)().foreach {
        case true =>
          checkpointerRR = Some(rr)
          assessCheckpointRecursive()
        case false =>
          sendRecursive(registry.lookup(receiveSyncMessageBinding, rr), changesSinceCP)
      }
    }

    while (true) {
      readLine() match {
        case add(n) =>
          set = set.add(n.toInt)
          println(s"Added $n")
          println(set.elements)
          processChangesForCheckpointing()
          sendDeltas()

        case remove(n) =>
          set = set.remove(n.toInt)
          println(s"Removed $n")
          println(set.elements)
          processChangesForCheckpointing()
          sendDeltas()

        case `clear` =>
          set = set.clear()
          processChangesForCheckpointing()
          sendDeltas()

        case `elements` =>
          println(set.elements)

        case `size` =>
          println(set.elements.size)

        case `exit` =>
          System.exit(0)

        case _ => println("Unknown command")
      }
    }
  }
}
