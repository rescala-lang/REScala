package rescala.crdts

import io.circe.generic.auto._
import loci.communicator.ws.akka.WS
import loci.registry.{Binding, Registry}
import loci.serializer.circe._
import loci.transmitter.RemoteRef
import rescala.crdts.pvars.Publishable._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import rescala.crdts.pvars._

import scala.language.higherKinds


//noinspection ScalaUnusedSymbol
object testSignalExpressions {
  def main(args: Array[String]): Unit = {






    val setBinding = Binding[PSet[Int]]("set")
    /* val counterBinding = Binding[PGrowOnlyCounter]("counter")(BindingBuilder.value[PGrowOnlyCounter](
      Marshallable.marshallable[GCounter, Int, GCounter](
        PGrowOnlyCounter.pGrowOnlyCounterTransmittable.asInstanceOf[Transmittable[GCounter,Int,GCounter]], implicitly[Serializable[Int]]
      ))
//    */
    println("running server")

    val (sr, s2) = { //server
      val registry = new Registry
      registry.listen(WS(1099))

      val l1 = PSet(Set(10))
      registry.bind(setBinding)(l1)

      (registry, l1)
    }

    println("running client")

    /**
      * { //client
      * val registry = new Registry
      * val connection: Future[RemoteRef] = registry.connect(WS("ws://localhost:1099/"))
      *connection.foreach { remote =>
      * val subscribedSig = registry.lookup(counterBinding, remote)
      * println("subscription")
      *subscribedSig.foreach(c => c.valueSignal.observe(v => println("client value: " + v)))
      * }
      *connection.failed.foreach(println)
      * *
      *
      * }
      **/

    val (cr, c1) = { //client2
      val registry = new Registry
      val connection: Future[RemoteRef] = registry.connect(WS("ws://localhost:1099/"))
      val remote: RemoteRef = Await.result(connection, Duration.Inf)
      val subscribedSig: Future[PSet[Int]] = registry.lookup(setBinding, remote)
      val set: PSet[Int] = Await.result(subscribedSig, Duration.Inf)
      (registry, set)
    }

    println("done")
    s2.add(20)
    c1.add(30)

    Thread.sleep(1000)
    Thread.sleep(1000)

    println("s2: " + s2.value)
    println("c1: " + c1.value)
    println("s2 == c1? " + (s2.value ==  c1.value))
    println("slept")
    sr.terminate()
  }
}

