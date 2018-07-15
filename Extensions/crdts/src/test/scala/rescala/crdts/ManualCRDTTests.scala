package rescala.crdts

import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.syntax._
import loci.communicator.ws.akka.WS
import loci.registry.{Binding, Registry}
import loci.serializer.circe._
import loci.transmitter.RemoteRef
import rescala.crdts.pvars.Publishable._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import rescala.crdts.pvars._
import rescala.crdts.statecrdts.sequences._
import rescala.crdts.statecrdts.StateCRDT

//noinspection ScalaUnusedSymbol
object testSignalExpressions {
  def main(args: Array[String]): Unit = {


    //var r = RGA("1","2")
    //var s = RGA[String]()

    val p = Vertex(4)
    val x = Vertex(5)
    val j = Vertex.start
    val k = Vertex.start[Int]
    println(j == k)
    val l = List(p, x, j)
    val f = Map[Vertex[Int], Vertex[Int]]() + (k -> x) + (x -> p)
    val json = f.asJson
    println(json)
    val m: Result[Map[Vertex[Int], Vertex[Int]]] = json.as[Map[Vertex[Int], Vertex[Int]]]
    println(f)
    println(m.getOrElse("error"))

    def merge[A, F](a: F, b: F)(implicit stateCRDT: StateCRDT[A, F]) = {
      stateCRDT.merge(a, b)
    }

    //s = merge(r,s)

    // r = r.addRight(Vertex.start, "4")
    // s = s.addRight(Vertex.start, "5")

    // println(merge(r,s).value)


    // test all bindings
    val counterBinding = Binding[PGrowOnlyCounter]("counter")
    val logBinding = Binding[PGrowOnlyLog[String]]("log")
    val gsetBinding = Binding[PGrowOnlySet[Int]]("gset")
    val setBinding = Binding[PSet[Int]]("set")
    val vertexListBinding = Binding[PVertexList[Int]]("vertexList")

    //Binding[PGrowOnlyLog[Int]]("log")(BindingBuilder.value[PGrowOnlyLog[Int]])
    // implicitly[Serializable[RGA[Int]]]
    /*
    val counterBinding = Binding[PGrowOnlyCounter]("counter")(BindingBuilder.value[PGrowOnlyCounter](
      Marshallable.marshallable[GCounter, Int, GCounter](
        Publishable.PVarTransmittable[GCounter,PGrowOnlyCounter].asInstanceOf[Transmittable[GCounter,Int,GCounter]], implicitly[Serializable[Int]]
      )))
    */
    println("running server")

    val (sr, s2) = { //server
      val registry = new Registry
      registry.listen(WS(1099))

      val l1 = PGrowOnlyLog(List("a"))
      registry.bind(logBinding)(l1)

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
      val subscribedSig: Future[PGrowOnlyLog[String]] = registry.lookup(logBinding, remote)
      val set: PGrowOnlyLog[String] = Await.result(subscribedSig, Duration.Inf)
      (registry, set)
    }

    println("done")
    s2.append("b")
    c1.append("c")

    Thread.sleep(1000)
    Thread.sleep(1000)

    println("s2: " + s2.value)
    println("c1: " + c1.value)
    println("s2 == c1? " + (s2.value == c1.value))
    println("slept")
    sr.terminate()
  }
}
