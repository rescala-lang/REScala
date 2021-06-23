package example.console

import loci._
import loci.communicator.tcp._
import loci.registry.Registry
import cats.implicits._
import loci.registry.Binding
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import loci.transmitter.Serializable
import loci.MessageBuffer
import java.nio.ByteBuffer
import scala.util.Try
import loci.serializer.jsoniterScala._

import loci.registry.Binding
//import loci.transmitter.IdenticallyTransmittable

import com.monovore.decline.Visibility.Partial
import com.monovore.decline.{Command, CommandApp, Opts}

object Main extends CommandApp(
      name = "cr",
      header = "test CRDTs on the commandline",
      main = Commandline.command.options
    )

object Commandline {

  val portArg = Opts.argument[Int](metavar = "port").withDefault(48693)
  val ipArg   = Opts.argument[String](metavar = "ip")

  val serveCommand = Opts.subcommand(
    name = "serve",
    help = "Start server"
  ) {
    (portArg).map { port =>
      Impl.serve(port)
    }
  }

  val clientCommand = Opts.subcommand(
    name = "connect",
    help = "Connect to server"
  ) {
    (portArg, ipArg).mapN { (port, ip) =>
      Impl.connect(ip, port)
    }
  }

  val command: Command[Unit] = Command(name = "conrep", header = "test CRTDs on the console") {
    serveCommand orElse clientCommand
  }

}

object Impl {
  val registry = new Registry

  //implicit def jsoniterBasedSerializable[T](implicit codec: JsonValueCodec[T]): Serializable[T] =
  //  new Serializable[T] {
  //    def serialize(value: T) = {
  //      val bytes = writeToArray(value)
  //      MessageBuffer.wrapByteBuffer(ByteBuffer.wrap(bytes))
  //    }
  //    def deserialize(value: MessageBuffer) =
  //      Try { readFromByteBuffer(value.asByteBuffer) }
  //  }

  implicit val StringRw: JsonValueCodec[String] = JsonCodecMaker.make
  //implicit val _Ts: IdenticallyTransmittable[String] = IdenticallyTransmittable()

  val versionBinding = Binding[String]("version")
  val reverseFunBinding = Binding[String => String]("reverseFun")

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global


  def serve(port: Int): Unit = {
    registry.bind(reverseFunBinding)((in: String) => in.reverse)
    println(registry.listen(TCP(port)))
    registry.bind(versionBinding)("0.0.1")
  }

  def connect(ip: String, port: Int): Unit = {
    registry.connect(TCP(ip, port)).foreach { rr =>
      println(s"connected to $rr")

      val version = registry.lookup(versionBinding, rr)

      version.foreach(println)

      val rfun = registry.lookup(reverseFunBinding, rr)

      println("lookup successful")

      rfun("hello world!").foreach(println)
    }

  }

}
