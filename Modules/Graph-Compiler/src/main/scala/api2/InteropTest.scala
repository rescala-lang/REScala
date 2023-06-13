package api2

import rescala.default.*

import scala.annotation.unused
import scala.io.StdIn.readLine

object InteropTest extends App {
  val toC = Evt[Int]()

  val toCString = Evt[String]()

  val remote = CompileGraph.withIO("interopTest")(toC, toCString) { (fromScala, fromScalaString) =>
    val localSource = CEvent.source[Int]

    @unused
    val localMap = localSource.map(_ / 2)

    @unused
    val strCopy = CEvent(fromScalaString.value)

    val plusOne = fromScala.map(_ + 1)

    val toScala = plusOne.map(_ * 2)

    val even = fromScala.map(_ % 2 == 0)

    toScala.observe(i => println(i))

    (toScala, even)
  }

  val tcpClient = new TCPClientConnector("localhost", 8000)
  tcpClient.connect()
  remote.setConnector(tcpClient)

  val (fromC, fromCBoolean) = remote.eventsFromListen()

  remote.startObserving()

  fromC.observe(i => println("fromC: " + i))
  fromCBoolean.observe(b => println("fromCBoolean: " + b))

  var loopCond = true

  while (loopCond) {
    val str = readLine()

    if (str.equals("exit")) {
      loopCond = false
    } else {
      str.toIntOption match {
        case Some(i) =>
          toC.fire(i)
        case None => println("Parse Error")
      }
    }
  }

  tcpClient.closeConnection()
}
