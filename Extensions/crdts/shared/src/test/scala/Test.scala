package rescala.deltacrdts

import loci.communicator.ws.akka.WS
import loci.registry.{Binding, Registry}
import loci.transmitter.RemoteRef
import org.scalatest.FreeSpec
import rescala.{default, reactives}
import rescala.default._
import rescala.distributables.LociDist
import rescala.distributables.LociDist.dfold
import rescala.parrp.ParRPStruct

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class DistLociTest extends FreeSpec {
  "very simple fold " in {


    //val messagesBinding = Binding[RGA[String] => Unit]("messages")


    val serverSource = Evt[String]

    val (serverRegistry, serverFold) = {

      val registry = new Registry
      registry.listen(WS(1099))

      val serverFold = (dfold(serverSource, registry)
      (List.empty[String])
      ((list, msg) => msg :: list)(Binding("messages")))

      (registry, serverFold)
    }

    println("running client")

    val clientSource = Evt[String]

    val (clientRegistry, clientFold) = {
      val registry = new Registry
      val connection: Future[RemoteRef] = registry.connect(WS("ws://localhost:1099/"))
      val remote: RemoteRef = Await.result(connection, Duration.Inf)

      val clientFold = makeListGraph(clientSource, registry)

      (registry, clientFold)
    }

    serverSource.fire("element 1")
    clientSource.fire("element 2")

    println(clientFold.now)
    println(serverFold.now)


    Thread.sleep(1000)
    Thread.sleep(1000)

    println(clientFold.now)
    println(serverFold.now)

    serverRegistry.terminate()
    clientRegistry.terminate()
  }

  def makeListGraph(clientSource: Evt[String], registry: Registry): Signal[List[String]] = {
    val theList = dfold(clientSource, registry)(List.empty[String])
    ((list, msg) => msg :: list)(Binding("messages")))
    theList
  }
}
