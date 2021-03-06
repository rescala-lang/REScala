package example.console

import cats.data.NonEmptyList
import cats.implicits._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import loci.communicator.tcp._
import loci.registry.{Binding, Registry}
import loci.serializer.jsoniterScala._
import loci.transmitter.transmittable.IdenticallyTransmittable
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.crdt.reactive.AWSet
import rescala.extra.lattices.delta.Codecs._

import scala.concurrent.Future
import scala.util.matching.Regex
import com.monovore.decline.{Command, CommandApp, Opts}
import loci.transmitter.{RemoteAccessException, RemoteRef}
import rescala.extra.lattices.delta.{Delta, UIJDLattice}

import java.util.concurrent.FutureTask
import scala.io.StdIn.readLine
import scala.util.{Failure, Success}

object ConsoleReplication extends CommandApp(
      name = "cr",
      header = "test CRDTs on the commandline",
      main = Commandline.command.options
    )

object Commandline {

  val idArg: Opts[String]                    = Opts.argument[String](metavar = "id")
  val listenPortArg: Opts[Int]               = Opts.argument[Int](metavar = "listenPort")
  val connectArg: Opts[NonEmptyList[String]] = Opts.arguments[String](metavar = "connectTo")

  val peerCommand: Opts[Unit] = Opts.subcommand(
    name = "peer",
    help = "Start peer"
  ) {
    val ipAndPort = """([\d.]*):(\d*)""".r

    (idArg, listenPortArg, connectArg).mapN {
      case (id, listenPort, connections) =>
        val ipsAndPorts = connections.map {
          case ipAndPort(ip, port) => (ip, port.toInt)
        }.toList

        new Peer(id, listenPort, ipsAndPorts).run()
    }
  }

  val listenPeerCommand: Opts[Unit] = Opts.subcommand(
    name = "listenpeer",
    help = "Start peer that only listens"
  ) {
    (idArg, listenPortArg).mapN {
      case (id, listenPort) => new Peer(id, listenPort, List()).run()
    }
  }

  val command: Command[Unit] = Command(name = "conrep", header = "test CRTDs on the console") {
    peerCommand orElse listenPeerCommand
  }

}

class Peer(id: String, listenPort: Int, connectTo: List[(String, Int)]) {
  val registry = new Registry

  implicit val IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  implicit val transmittableAWSet: IdenticallyTransmittable[AWSet.State[Int, DietMapCContext]] =
    IdenticallyTransmittable()

  val receiveDeltaBinding
      : Binding[AWSet.State[Int, DietMapCContext] => Unit, AWSet.State[Int, DietMapCContext] => Future[Unit]] =
    Binding[AWSet.State[Int, DietMapCContext] => Unit]("receiveDelta")

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  val add: Regex       = """add (\d+)""".r
  val remove: Regex    = """remove (\d+)""".r
  val clear: String    = "clear"
  val elements: String = "elements"
  val exit: String     = "exit"

  var set: AWSet[Int, DietMapCContext] = AWSet(id)

  def sendDeltas(): Unit = {
    registry.remotes.foreach { rr =>
      val remoteReceiveDelta = registry.lookup(receiveDeltaBinding, rr)

      set.deltaBuffer.collect {
        case Delta(replicaID, deltaState) if replicaID != rr.toString => deltaState
      }.reduceOption(UIJDLattice[AWSet.State[Int, DietMapCContext]].merge).foreach(remoteReceiveDelta)
    }

    set = set.resetDeltaBuffer()
  }

  def run(): Unit = {
    registry.bindSbj(receiveDeltaBinding) { (remoteRef: RemoteRef, deltaState: AWSet.State[Int, DietMapCContext]) =>
      val delta = Delta(remoteRef.toString, deltaState)
      set = set.applyDelta(delta)
      sendDeltas()
      set = set.resetDeltaBuffer()

      println("Received Delta")
      println(set.elements)
    }

    println(registry.listen(TCP(listenPort)))

    var remoteToAddress = Map[RemoteRef, (String, Int)]()

    def connectToRemote(address: (String, Int)): Unit = address match {
      case (ip, port) =>
        new FutureTask[Unit](() => {
          def attemptReconnect(): Unit = {
            registry.connect(TCP(ip, port)).onComplete {
              case Success(value) =>
                remoteToAddress = remoteToAddress.updated(value, (ip, port))
                return
              case Failure(_) =>
                Thread.sleep(1000)
                attemptReconnect()
            }
          }

          attemptReconnect()
        }).run()
    }

    connectTo.foreach(connectToRemote)

    registry.remoteLeft.monitor { rr =>
      remoteToAddress.get(rr) match {
        case Some(address) =>
          remoteToAddress = remoteToAddress.removed(rr)

          connectToRemote(address)

        case None =>
      }
    }

    def sendRecursive(
        remoteReceiveDelta: AWSet.State[Int, DietMapCContext] => Future[Unit],
        delta: AWSet.State[Int, DietMapCContext]
    ): Unit = {
      new FutureTask[Unit](() => {
        def attemptSend(
            atoms: Iterable[AWSet.State[Int, DietMapCContext]],
            merged: AWSet.State[Int, DietMapCContext]
        ): Unit = {
          remoteReceiveDelta(merged).failed.foreach {
            case e: RemoteAccessException => e.reason match {
                case RemoteAccessException.RemoteException(name, _) if name.contains("JsonReaderException") =>
                  val a =
                    if (atoms.isEmpty) UIJDLattice[AWSet.State[Int, DietMapCContext]].decompose(merged)
                    else atoms

                  val atomsSize = a.size

                  val firstHalf  = a.take(atomsSize / 2)
                  val secondHalf = a.drop(atomsSize / 2)

                  attemptSend(firstHalf, firstHalf.reduce(UIJDLattice[AWSet.State[Int, DietMapCContext]].merge))
                  attemptSend(secondHalf, secondHalf.reduce(UIJDLattice[AWSet.State[Int, DietMapCContext]].merge))
                case _ => e.printStackTrace()
              }
            case e => e.printStackTrace()
          }
        }

        attemptSend(List(), delta)
      }).run()
    }

    registry.remoteJoined.monitor { rr =>
      sendRecursive(registry.lookup(receiveDeltaBinding, rr), set.state)
    }

    while (true) {
      readLine() match {
        case add(n) =>
          set = set.add(n.toInt)
          println(s"Added $n")
          println(set.elements)
          sendDeltas()

        case remove(n) =>
          set = set.remove(n.toInt)
          println(s"Removed $n")
          println(set.elements)
          sendDeltas()

        case `clear` =>
          set = set.clear()
          sendDeltas()

        case `elements` =>
          println(set.elements)

        case `exit` =>
          System.exit(0)

        case _ => println("Unknown command")
      }
    }
  }
}
