package rescala.api2

import rescala.default.*
import StandardBundle.compileGraph
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

import scala.io.StdIn.readLine
import java.net.InetSocketAddress
import java.nio.channels.*
import java.nio.ByteBuffer
import scala.concurrent.Future

object InteropTest extends App {
  val toC = Evt[Int]()

//  inline def test[T <: Product](): T = {
//    // create Events like this for every MetaBundle Event "returned" from compileGraph
//    Events.fromCallback { cb =>
//      // register cb as callback when receiving network data
//    }
//  }

  compileGraph("interopTest") {
    import StandardBundle.*

    val fromScala = Event(toC.value.map(_ + 1))

    val toScala = fromScala.map(_ * 2)

    toScala.observe(i => println(i))

    Tuple1(toScala)
  }

  given JsonValueCodec[Tuple1[Option[Int]]] = JsonCodecMaker.make
  given JsonValueCodec[Int] = JsonCodecMaker.make

  val fromC = Evt[String]()
  fromC.observe(s => println("fromC fired: " + s))

  val client = AsynchronousSocketChannel.open()
  client.connect(InetSocketAddress("localhost", 8000)).get()

  import scala.concurrent.ExecutionContext.Implicits.global

  class ReadHandler(cb: String => Unit) extends CompletionHandler[Integer, ByteBuffer]() {
    override def completed(result: Integer, attachment: ByteBuffer): Unit = {
      val buffer = attachment
      buffer.flip()
      val bytes = Array.ofDim[Byte](buffer.remaining())
      buffer.get(bytes)
      val str = new String(bytes)
      if (!str.isBlank) {
        println("Raw string: " + new String(bytes))
        cb(new String(bytes))
        println("After firing")
      }

      buffer.clear()
      client.read(buffer, buffer, this)
    }

    override def failed(exc: Throwable, attachment: ByteBuffer): Unit = {
      println("failed called with exception: ")
      exc.printStackTrace()
    }
  }

  val recvBuffer = ByteBuffer.allocate(100)

  val fromCString = Events.fromCallback[String] { cb =>
    client.read(recvBuffer, recvBuffer, new ReadHandler(cb))
  }.event
  fromCString.observe(s => println("fromCString: " + s))

  val fromCParsed = fromCString.map(s =>
//    println("Before readFromString")
//    val str = readFromString[Tuple1[Option[Int]]](s)
//    println("After readFromString")
    Tuple1(Some(3))
  )
  fromCParsed.observe(t => println("fromCParsed: " + t))

  val fromCCollected = fromCParsed.collect {
    case Tuple1(Some(i)) => i
  }
  fromCCollected.observe(i => println("fromCCollected: " + i))

//  Event {
//    (toC.value, otherEvent.value)
//  }.observe

  val toCSerialized = toC.map(v => writeToString(v))

  Event {
    Some(List(toCSerialized.value))
  }.observe { t =>
    val msg = t.map {
      case None => "null"
      case Some(str) => str
    }.mkString("[", ", ", "]\n")

    client.write(ByteBuffer.wrap(msg.getBytes))
  }

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
}
