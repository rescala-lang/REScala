import java.util.Scanner

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle

import statecrdts._
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.actor.Props


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

/**
  * Created by julian on 26.07.17.
  */
object ChatApp {


  def main2(args: Array[String]): Unit = args(0) match {
    case "Alice" => startup("Alice", "2251")
    case "Bob" => startup("Bob", "2252")
    case "Charlie" => startup("Charlie", "2253")
  }

  def startup(name: String, port: String): Unit = {
    val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
      withFallback(ConfigFactory.load())

    // Create an Akka system
    val system = ActorSystem("ClusterSystem", config)
    // Create an actor that handles cluster domain events
    //system.actorOf(Props[SimpleClusterListener], name = "clusterListener")
    run(name)
  }

  def run(name: String): Unit ={
    val s = new Scanner(System.in)
    System.out.println(s"Hello $name!")

    while (true) {
      print("> ")
      var msg = s.nextLine.trim
      println(s"[$name] $msg")
    }
  }

}
