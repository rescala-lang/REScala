package replication.checkpointing.decentral

import loci.communicator.tcp.TCP
import loci.registry.Registry
import loci.transmitter.RemoteRef

import java.util.concurrent.FutureTask
import scala.util.{Failure, Success}

abstract class Peer {
  val listenPort: Int
  val connectTo: List[(String, Int)]

  val registry = new Registry

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  var remoteToAddress: Map[RemoteRef, (String, Int)] = Map()

  def connectToRemote(address: (String, Int)): Unit = address match {
    case (ip, port) =>
      new FutureTask[Unit](() => {
        def attemptConnect(): Unit = {
          registry.connect(TCP(ip, port)).onComplete {
            case Success(value) =>
              remoteToAddress = remoteToAddress.updated(value, (ip, port))
            case Failure(_) =>
              Thread.sleep(1000)
              attemptConnect()
          }
        }

        attemptConnect()
      }).run()
  }

  def setupConnectionHandling(): Unit = {
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
    ()
  }
}
