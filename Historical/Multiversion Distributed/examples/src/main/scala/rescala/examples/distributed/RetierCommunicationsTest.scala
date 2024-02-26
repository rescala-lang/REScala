package reactives.examples.distributed

import reactives.fullmv.DistributedFullMVApi.{FullMVEngine, Signal, scheduler, Var, Signals}
import loci.communicator.tcp._
import loci.registry.{Binding, Registry}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Bindings {

  import loci.serializer.jsoniterScala._
  import com.github.plokhotnyuk.jsoniter_scala.macros._
  import com.github.plokhotnyuk.jsoniter_scala.core._

  given JsonValueCodec[scala.Tuple2[
    scala.Long,
    scala.Tuple3[
      java.lang.String,
      scala.collection.immutable.List[scala.Tuple4[
        scala.Long,
        scala.Int,
        scala.Option[scala.Tuple2[reactives.fullmv.CaseClassTransactionSpanningTreeNode[scala.Tuple2[
          reactives.fullmv.mirrors.Host.GUID,
          reactives.fullmv.TurnPhase.Type
        ]], scala.Int]],
        scala.Option[scala.Tuple2[scala.Option[scala.Int], scala.Option[scala.Array[scala.Byte]]]]
      ]],
      scala.Array[scala.Byte]
    ]
  ]] = JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  given JsonValueCodec[Int] = JsonCodecMaker.make
  import reactives.fullmv.DistributedFullMVApi.ReactiveTransmittable._
  implicit val host: FullMVEngine = scheduler

  val testBinding                                                    = Binding[Int => Int]("test")
  val variableBinding: Binding[Signal[(Int)], Future[Signal[(Int)]]] = Binding[Signal[Int]]("variable")
}

object Main1 extends App {
  var connected = false
  def test(x: Int) = {
    println(s"processing test($x)")
    Thread.sleep(x)
    if (x == 42) {
      connected = true
      println("Now running updates. Press <Enter> to stop.")
    }
    2 * x
  }
  val variable = Var(0)

  val registry = new Registry
  registry.listen(TCP(1099))

  registry.bind(Bindings.testBinding)(test)
  registry.bind(Bindings.variableBinding)(variable)

  println("Awaiting Connection. Press <Enter> to terminate.")
  while (System.in.available() == 0) {
    if (connected) {
      variable transform { x =>
        println(s"increment($x)")
        x + 1
      }
    } else {
      Thread.sleep(1000)
    }
//    Thread.sleep(1000)
  }
  registry.terminate()
  println("Updates stopped, registry shut down.")
}

object Main2 extends App {
  val registry = new Registry
  val remote   = Await result (registry.connect(TCP("localhost", 1099)), Duration.Inf)

  import scala.concurrent.ExecutionContext.Implicits._
//  val test: Int => Future[Int] = registry.lookup[Int => Int]("test", remote)
  val test: Int => Future[Int] = registry.lookup(Bindings.testBinding, remote)
  test(1000).onComplete(x => println("test(1000)" + x))
  test(50).onComplete(x => println("test(50)" + x))

//  val signal: Signal[Int] = Await result (registry.lookup[Signal[Int]]("variable", remote), Duration.Inf)
  val signal: Signal[Int] = Signals.fromFuture(registry.lookup(Bindings.variableBinding, remote)).flatten
  signal observe println

  test(42).onComplete(x => println("connect confirmation: " + x))

  println("Connected. Press <Enter> to disconnect.")
  while (System.in.available() == 0) { Thread.sleep(10) }
  registry.terminate()
  println("Disconnected, registry shut down.")
}
