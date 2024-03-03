package todo

import channel.{ArrayMessageBuffer, MessageBuffer, OutChan}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromSubArray, writeToArray}
import loci.communicator.webrtc.WebRTCConnection
import rdts.base.Lattice
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import reactives.default.*

import java.nio.charset.StandardCharsets

object GlobalRegistry {

  case class Listener[A](evt: Evt[Dotted[A]], codec: JsonValueCodec[Dotted[A]])

  private var listeners: Map[String, Listener[?]] = Map.empty

  private var channels: List[OutChan] = Nil

  def addConnection(connection: WebRTCConnection): Unit = {
    println(s"adding channel")
    channels = connection :: channels
  }

  def handle(message: MessageBuffer): Unit = {
    val messageArray = message.asArray
    val pos          = messageArray.indexOfSlice("\r\n\r\n".getBytes(StandardCharsets.UTF_8))
    if pos <= 0 then println(s"Received invalid message")
    else
      println(s"handling received message")
      val name = messageArray.slice(0, pos)
      listeners.get(new String(name, StandardCharsets.UTF_8)).foreach: listener =>
        val decoded = readFromSubArray(messageArray, pos + 4, messageArray.length)(listener.codec)
        listener.evt.fire(decoded)
  }

  def subscribe[A](name: String)(using
      codec: JsonValueCodec[Dotted[A]],
      lattice: Lattice[Dotted[A]]
  ): Event[Dotted[A]] = {

    val incoming: Evt[Dotted[A]] = Evt()

    listeners = listeners.updated(name, Listener(incoming, codec))

    incoming
  }

  def subscribeBranch[A](name: String)(using
      codec: JsonValueCodec[Dotted[A]],
      lattice: Lattice[Dotted[A]]
  ): Fold.Branch[DeltaBuffer[Dotted[A]]] =
    subscribe(name).act[DeltaBuffer[Dotted[A]]](delta => Fold.current.clearDeltas().applyDelta(delta))

  def encode[A: JsonValueCodec](name: String, value: A) =
    ArrayMessageBuffer(s"$name\r\n\r\n".getBytes(StandardCharsets.UTF_8) ++ writeToArray(value))

  def publish[A](name: String, reactive: Signal[DeltaBuffer[Dotted[A]]])(using JsonValueCodec[Dotted[A]]) = {
    println(s"publishing $name")
    val initial = encode(name, reactive.now.state)
    channels.foreach(c => c.send(initial))

    reactive.observe: db =>
      println(s"publishing changes of $name to $channels")
      db.deltaBuffer.foreach: delta =>
        println(s"change is $delta")
        val msg = encode(name, delta)
        channels.foreach(c => c.send(msg).run(println))

  }
}
