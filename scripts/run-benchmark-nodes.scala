//> using scala 3.5.2
//> using dep de.rmgk.slips::options:0.9.0
//> using dep de.rmgk.slips::script:0.9.0

import de.rmgk.options.*
import de.rmgk.script.*

import java.nio.file.Path

object RunBenchNodes {
  def main(args: Array[String]): Unit = {

    parseArguments(args.toList) {

      val jarpath = named[Path]("--jars", "").value

      val nodes = named[Int]("--nodes", "").value

      val range = Range.inclusive(1, nodes)

      val clusterIds = range.map(n => s"NODE$n")

      def clientPort(n: Int)  = 8009 + n
      def clusterPort(n: Int) = 50000 + n

      val processes = range.map { n =>

        val name = s"NODE$n"

        val prior = Range.inclusive(1, n).map(r => s"localhost:${clusterPort(r)}")

        val proc = process"""java --class-path ${jarpath.toString + "/*"} probench.cli node --name $name
                  --listen-client-port ${clientPort(n)} --listen-peer-port
                  ${clusterPort(
            n
          )} --cluster $prior --initial-cluster-ids ${clusterIds}""".inheritIO().redirectOutput(Path.of(
          s"$n.out"
        ).toFile).start().nn

        println(s"starting process $n")

        Thread.sleep(100)

        proc

      }

      println(s"press enter to KILLAALL")

      scala.io.StdIn.readLine()

      processes.foreach((p: Process) => p.destroyForcibly())

    }
  }
}
