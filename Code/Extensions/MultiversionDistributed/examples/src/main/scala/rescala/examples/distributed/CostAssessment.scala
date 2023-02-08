package rescala.examples.distributed

import rescala.fullmv.DistributedFullMVApi.{FullMVEngine, ReactiveLocalClone, ReactiveTransmittable, Signal, Var}
import loci.communicator.tcp.TCP
import loci.registry.{Binding, Registry}
import tests.rescala.fullmv.transmitter.TransmitterTestsPortManagement
import tests.rescala.testtools.Spawn

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object CostAssessment {
  def merge(maps: Map[String, Set[Int]]*): Map[String, Set[Int]] = {
    maps.reduce { (m1, m2) =>
      m2.foldLeft(m1) {
        case (map, (key, values)) =>
          map + (key -> (map.get(key) match {
            case Some(values2) => values ++ values2
            case None          => values
          }))
      }
    }
  }
  def isGlitched(v: Map[String, Set[Int]]): Boolean = v.exists(_._2.size > 1)

  case class Side(host: FullMVEngine, step: () => Unit)
  case class Graph(
      left: Side,
      right: Side,
      topHost: FullMVEngine,
      topMerge: Signal[Map[String, Set[Int]]]
  )
  type GraphRunner[R] = (Int, Graph => R) => (R, List[Map[String, Set[Int]]])

  def distributedRunner[R]: GraphRunner[R] = { (length: Int, run: (Graph => R)) =>
    class Host(name: String) extends FullMVEngine(10.second, name) {
      val registry   = new Registry
      def shutdown() = registry.terminate()

      import loci.serializer.jsoniterScala._
      import ReactiveTransmittable._
      import com.github.plokhotnyuk.jsoniter_scala.macros._
      import com.github.plokhotnyuk.jsoniter_scala.core._

      given JsonValueCodec[scala.Tuple2[
        scala.Long,
        scala.Tuple3[java.lang.String, scala.collection.immutable.List[scala.Tuple4[
          scala.Long,
          scala.Int,
          scala.Option[scala.Tuple2[rescala.fullmv.CaseClassTransactionSpanningTreeNode[scala.Tuple2[
            rescala.fullmv.mirrors.Host.GUID,
            rescala.fullmv.TurnPhase.Type
          ]], scala.Int]],
          scala.Option[scala.Tuple2[
            scala.Option[scala.collection.immutable.Map[java.lang.String, scala.collection.immutable.Set[scala.Int]]],
            scala.Option[scala.Array[scala.Byte]]
          ]]
        ]], scala.Array[scala.Byte]]
      ]] = JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
      implicit val host: this.type = this
      def binding(i: Int)          = Binding[Signal[Map[String, Set[Int]]]](s"binding-$i")
    }

    class SideHost(name: String) extends Host(name) {
      val port: Int = TransmitterTestsPortManagement.getFreePort()
      registry.listen(TCP(port))

      val source: Var[Int]                            = Var(0)
      val taggedSource: Signal[Map[String, Set[Int]]] = source.map(v => Map(name -> Set(v)))
      registry.bind(binding(0))(taggedSource)

      def step(): Unit = source.transform(_ + 1)
    }

    val leftHost = new SideHost("left-retiercomm")
    try {
      val rightHost = new SideHost("right-retiercomm")
      try {

        var leftOutput  = leftHost.taggedSource
        var rightOutput = rightHost.taggedSource

        for (i <- 1 to length) {
          val leftOutputStatic  = leftOutput
          val rightOutputStatic = rightOutput

          leftOutput = {
            import leftHost._
            val remoteRight     = Await.result(registry.connect(TCP("localhost", rightHost.port)), timeout)
            val outputFromRight = Await.result(registry.lookup(binding(i - 1), remoteRight), timeout)
            val merged          = Signal { merge(leftOutputStatic(), outputFromRight()) }
            registry.bind(binding(i))(merged)
            merged
          }

          rightOutput = {
            import rightHost._
            val remoteLeft     = Await.result(registry.connect(TCP("localhost", leftHost.port)), timeout)
            val outputFromLeft = Await.result(registry.lookup(binding(i - 1), remoteLeft), timeout)
            val merged         = Signal { merge(outputFromLeft(), rightOutputStatic()) }
            registry.bind(binding(i))(merged)
            merged
          }
        }

        val topHost = new Host("top-retiercomm")
        try {
          import topHost._

          val remoteLeft     = Await.result(registry.connect(TCP("localhost", leftHost.port)), timeout)
          val mergeFromLeft  = Await.result(registry.lookup(binding(length), remoteLeft), timeout)
          val remoteRight    = Await.result(registry.connect(TCP("localhost", rightHost.port)), timeout)
          val mergeFromRight = Await.result(registry.lookup(binding(length), remoteRight), timeout)
          val topMerge: Signal[Map[String, Set[Int]]] = Signal { merge(mergeFromLeft(), mergeFromRight()) }

          var violations: List[Map[String, Set[Int]]] = Nil
          topMerge.observe { v => if (isGlitched(v)) violations = v :: violations }

          val left   = Side(leftHost, () => leftHost.step())
          val right  = Side(rightHost, () => rightHost.step())
          val result = run(Graph(left, right, topHost, topMerge))
          (result, violations)
        } finally {
          topHost.shutdown()
        }
      } finally {
        rightHost.shutdown()
      }
    } finally {
      leftHost.shutdown()
    }
  }

  def localmirrorRunner[R]: GraphRunner[R] = { (length: Int, run: (Graph => R)) =>
    class SideHost(name: String) extends FullMVEngine(Duration.Zero, name) {
      val source: Var[Int]                            = Var(0)
      val taggedSource: Signal[Map[String, Set[Int]]] = source.map(v => Map(name -> Set(v)))
      def step(): Unit                                = source.transform(_ + 1)
    }

    val leftHost  = new SideHost("left-localmirror")
    val rightHost = new SideHost("right-localmirror")

    var leftOutput  = leftHost.taggedSource
    var rightOutput = rightHost.taggedSource

    for (i <- 1 to length) {
      val leftOutputStatic  = leftOutput
      val rightOutputStatic = rightOutput
      leftOutput = {
        import leftHost._
        val outputFromRight = ReactiveLocalClone(rightOutputStatic, leftHost)
        Signal { merge(leftOutputStatic(), outputFromRight()) }
      }

      rightOutput = {
        import rightHost._
        val outputFromLeft = ReactiveLocalClone(leftOutputStatic, rightHost)
        Signal { merge(outputFromLeft(), rightOutputStatic()) }
      }
    }

    val topHost = new FullMVEngine(Duration.Zero, "top-localmirror")
    import topHost._

    val mergeFromLeft                           = ReactiveLocalClone(leftOutput, topHost)
    val mergeFromRight                          = ReactiveLocalClone(rightOutput, topHost)
    val topMerge: Signal[Map[String, Set[Int]]] = Signal { merge(mergeFromLeft(), mergeFromRight()) }

    var violations: List[Map[String, Set[Int]]] = Nil
    topMerge.observe { v => if (isGlitched(v)) violations = v :: violations }

    val left   = Side(leftHost, () => leftHost.step())
    val right  = Side(rightHost, () => rightHost.step())
    val result = run(Graph(left, right, topHost, topMerge))
    (result, violations)
  }

  def localRunner[R]: GraphRunner[R] = { (length: Int, run: (Graph => R)) =>
    val engine = new FullMVEngine(Duration.Zero, "local")
    import engine._

    val leftSource: Var[Int] = Var(0)
    var leftOutput           = leftSource.map(v => Map("local-left" -> Set(v)))

    val rightSource: Var[Int] = Var(0)
    var rightOutput           = rightSource.map(v => Map("local-right" -> Set(v)))

    for (i <- 1 to length) {
      val leftOutputStatic  = leftOutput
      val rightOutputStatic = rightOutput

      val fakeMirrorRightOutput = rightOutputStatic.map(identity)
      leftOutput = Signal { merge(leftOutputStatic(), fakeMirrorRightOutput()) }

      val fakeMirrorLeftOutput = leftOutputStatic.map(identity)
      rightOutput = Signal { merge(fakeMirrorLeftOutput(), rightOutputStatic()) }
    }

    val fakeMirrorLeftMerge                     = leftOutput.map(identity)
    val fakeMirrorRightMerge                    = rightOutput.map(identity)
    val topMerge: Signal[Map[String, Set[Int]]] = Signal { merge(fakeMirrorLeftMerge(), fakeMirrorRightMerge()) }

    var violations: List[Map[String, Set[Int]]] = Nil
    topMerge.observe { v => if (isGlitched(v)) violations = v :: violations }

    val left   = Side(engine, () => leftSource.transform(_ + 1))
    val right  = Side(engine, () => rightSource.transform(_ + 1))
    val result = run(Graph(left, right, engine, topMerge))
    (result, violations)
  }

  def harness(runner: (Long, Graph) => (Int, Int)) = {
    (graph: Graph) =>
      (1 to 10).foreach { i =>
        val counts @ (left, right) = runner(System.currentTimeMillis() + 1000L, graph)
        val leftTime               = 1000d / left
        val rightTime              = 1000d / right
        println(
          f"warmup $i%3d: $counts%s iterations ($leftTime%.4f ms/op and $rightTime%.4f ms/op, avg ${(leftTime + rightTime) / 2}%.4f)"
        )
      }
      val iterations = (1 to 20).flatMap { i =>
        val counts @ (left, right) = runner(System.currentTimeMillis() + 1000L, graph)
        val leftTime               = 1000d / left
        val rightTime              = 1000d / right
        println(
          f"iteration $i%3d: $counts%s iterations ($leftTime%.4f ms/op and $rightTime%.4f ms/op, avg ${(leftTime + rightTime) / 2}%.4f)"
        )
        Seq(leftTime, rightTime)
      }
      iterations.sum / iterations.size
  }

  def conflictFree(until: Long, graph: Graph): (Int, Int) = {
    val res = Spawn {
      var count = 0
      while (System.currentTimeMillis() < until) {
        graph.left.step()
        graph.right.step()
        count += 2
      }
      count
    }.await(until - System.currentTimeMillis() + 500L)
    (res, res)
  }

  def conflicting(until: Long, graph: Graph): (Int, Int) = {
    val left = Spawn {
      var count = 0
      while (System.currentTimeMillis() < until) {
        graph.left.step()
        count += 1
      }
      count
    }
    val right = Spawn {
      var count = 0
      while (System.currentTimeMillis() < until) {
        graph.right.step()
        count += 1
      }
      count
    }
    left.await(until - System.currentTimeMillis() + 500L) -> right.await(until - System.currentTimeMillis() + 500L)
  }

  def main(args: Array[String]): Unit = {
    val length =
      if (args.length == 0) {
        println("using default length 1")
        1
      } else {
        println("using length " + args(0))
        Integer.parseInt(args(0))
      }

    def measure[R](rowId: String, run: => (R, List[Map[String, Set[Int]]])): (String, R) = {
      println(s"Running $rowId...")
      val (result, violations) = run
      if (violations.isEmpty) {
        println("no violations => OK")
      } else {
        println("there were violations:")
        println(violations.mkString("\n"))
      }
      rowId -> result
    }
    val results = Seq(
      measure("simple local\tsequential", localRunner(length, harness(conflictFree))),
      measure("simple local\tconcurrent", localRunner(length, harness(conflicting))),
//        measure("local mirror\tsequential", localmirrorRunner(length, harness(conflictFree))),
//        measure("local mirror\tconcurrent", localmirrorRunner(length, harness(conflicting))),
      measure("distributed\tsequential", distributedRunner(length, harness(conflictFree))),
      measure("distributed\tconcurrent", distributedRunner(length, harness(conflicting)))
    )
    println(" === RESULTS === ")
    println("graph origin\tconflicts\tavg ms/op")
    results.foreach {
      case (rowId, result) =>
        println(f"$rowId%s\t$result%.4f")
    }
  }
}
