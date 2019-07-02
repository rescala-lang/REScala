import loci.communicator.ws.akka.WS
import loci.registry.{Binding, Registry}
import loci.transmitter.RemoteRef
import org.scalatest.FreeSpec
import rescala.default._
import rescala.distributables.LociDist.dfold
import rescala.lattices.sequences.RGA.RGA
import loci.serializer.circe._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.syntax._

class DistLociTest extends FreeSpec {
  "very simple fold " in {


    //val messagesBinding = Binding[RGA[String] => Unit]("messages")


    val serverSource = Evt[String]

    val (serverRegistry, serverFold) = {

      val registry = new Registry
      registry.listen(WS(1099))

      val serverFold = makeListGraph(serverSource, registry)

      (registry, serverFold)
    }

    println("running client")

    val clientSource = Evt[String]

    val (clientRegistry, clientFold) = {
      val registry = new Registry
      val connection: Future[RemoteRef] = registry.connect(WS("ws://localhost:1099/"))
      Await.result(connection, Duration.Inf)

      val clientFold = makeListGraph(clientSource, registry)

      (registry, clientFold)
    }

    clientSource.fire("element 2")
    serverSource.fire("element 1")

    println(clientFold.now)
    println(serverFold.now)


    Thread.sleep(1000)
    Thread.sleep(1000)

    println(clientFold.now)
    println(serverFold.now)

    serverRegistry.terminate()
    clientRegistry.terminate()
  }

  def makeListGraph(clientSource: Evt[String], registry: Registry): Signal[String] = {
    val theList = dfold(clientSource)(List.empty[String])((list, msg) => list :+ msg)(registry, Binding[RGA[String] => Unit]("messages"))
    val size = theList.map(l => l.size - 1)

    Signal {
      theList.value(size.value)
    }
  }
}
