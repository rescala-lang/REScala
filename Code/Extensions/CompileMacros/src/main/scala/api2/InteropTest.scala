package rescala.api2

import rescala.default.*
import StandardBundle.{compileRemoteGraphWithIO, compileRemoteGraphWithInput, compileRemoteGraphWithOutput}
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

import scala.io.StdIn.readLine
import java.net.InetSocketAddress
import java.nio.channels.*
import java.nio.ByteBuffer
import scala.concurrent.Future

object InteropTest extends App {
  val toC = Evt[Int]()

  val toCString = Evt[String]()

  given c1: JsonValueCodec[(Option[Int], Option[String])] = JsonCodecMaker.make

  given c2: JsonValueCodec[(Option[Int], Option[Boolean])] = JsonCodecMaker.make

  val remote = compileRemoteGraphWithIO("interopTest")(toC, toCString) { (fromScala, fromScalaString) =>
    import StandardBundle.*

    val strCopy = Event(fromScalaString.value)

    val plusOne = fromScala.map(_ + 1)

    val toScala = plusOne.map(_ * 2)

    val even = fromScala.map(_ % 2 == 0)

    toScala.observe(i => println(i))

    (toScala, even)
  }

  remote.connect("localhost", 8000)

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

  remote.closeConnection()
}
