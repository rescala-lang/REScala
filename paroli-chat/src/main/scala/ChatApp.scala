import java.util.Scanner

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle


import akka.cluster.Cluster
import akka.cluster.ClusterEvent._
import statecrdts._
import distributionengine._
import com.typesafe.config.ConfigFactory
import akka.actor._
import akka.actor.Props

import scala.tools.jline

/*
object HelloStageDemo extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Stage"
    width = 600
    height = 450
    scene = new Scene {
      fill = White
      content = new Rectangle {
        x = 25
        y = 40
        width = 100
        height = 100
        fill <== when (hover) choose Green otherwise Red
      }
    }
  }
}
*/

/**
  * Created by julian on 26.07.17.
  */
object ChatApp {
  val console = new jline.console.ConsoleReader()

  def main(args: Array[String]): Unit = args(0) match {
    case "Alice" => startup("Alice", "2251")
    case "Bob" => startup("Bob", "2252")
    case "Charlie" => startup("Charlie", "2253")
  }

  def startup(name: String, port: String): Unit = {
    val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
      withFallback(ConfigFactory.load())

    // Create an Akka system
    val system = ActorSystem("ClusterSystem", config)

    val cluster = Cluster(context.system)
    val host: ActorRef = system.actorOf(DistributionEngine.props("Host1", lookupServer), "Host1")
    // Create an actor that handles cluster domain events
    //system.actorOf(Props[SimpleClusterListener], name = "clusterListener")
    run(name)
  }

  def run(name: String): Unit = {
    var history: List[String] = List()
    history = history :+ s"System: Hello $name!"

    while (true) {
      drawInterface(name, history)

      // print input prompt
      val msg: String = console.readLine(s"[$name]: ")
      history = history :+ (s"[$name] $msg")
    }

    def drawInterface(name: String, log: List[String]): Unit = {
      // clear screen twice to make some room between current interface and older interfaces
      (0 to 50).foreach(_ => println())
      console.clearScreen()

      val terminalWidth = jline.TerminalFactory.get.getWidth
      val terminalHeight = jline.TerminalFactory.get.getHeight
      val header = (1 to terminalWidth).map(_ => "_").mkString
      val footer = (1 to terminalWidth).map(_ => "_").mkString

      var outputLines = 3

      // print header
      println(header)

      // print history
      log.foreach {
        case s: String => {
          // slice output string into substrings of the same length as the terminal
          if (s.length > terminalWidth) {
            s.grouped(terminalWidth).foreach(slice => {
              println(slice)
              outputLines += 1
            })
          }
          else {
            println(s)
            outputLines += 1
          }
        }
      }

      // print empty lines if the terminal window is bigger than the current history to place footer at the bottom of the screen
      if (terminalHeight > log.length) (1 to terminalHeight - outputLines).foreach(_ => println())
      println(footer)
    }
  }

}
