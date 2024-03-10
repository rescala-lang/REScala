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

  case class Valued[A](signal: Signal[DeltaBuffer[Dotted[A]]], codec: JsonValueCodec[Dotted[A]])

  private var listeners: Map[String, Listener[?]] = Map.empty

  private var currentValues: Map[String, Valued[?]] = Map.empty

  private var buffer: Map[String, List[(Int, Array[Byte])]] = Map.empty

  private var channels: List[OutChan] = Nil

  def addConnection(connection: WebRTCConnection): Unit = {
    println(s"adding channel")
    channels = connection :: channels

    currentValues.foreach: (id, cw) =>
      val enc = encode(id, cw.signal.now.state)(using cw.codec)
      channels.foreach: chan =>
        println(s"sending existing data for $id to new channel")
        chan.send(enc).run(using ())(println)
  }

  def handle(message: MessageBuffer): Unit = {
    val messageArray = message.asArray
    val pos          = messageArray.indexOfSlice("\r\n\r\n".getBytes(StandardCharsets.UTF_8))
    if pos <= 0 then println(s"Received invalid message")
    else
      val name = new String(messageArray.slice(0, pos), StandardCharsets.UTF_8)
      println(s"handling received message for $name")
      listeners.get(name) match
        case Some(listener) =>
          val decoded = readFromSubArray(messageArray, pos + 4, messageArray.length)(listener.codec)
          listener.evt.fire(decoded)
        case None =>
          println(s"no listener for $name, buffering")
          buffer = buffer.updatedWith(name) {
            case None    => Some(List((pos + 4, messageArray)))
            case Some(l) => Some((pos + 4, messageArray) :: l)
          }
  }

  def unbuffer(name: String): Unit = {
    (buffer.get(name), listeners.get(name)) match
      case (Some(elements), Some(listener)) =>
        println(s"unbuffering $name: ${elements.length}")
        elements.foreach: (offset, bytes) =>
          val decoded = readFromSubArray(bytes, offset, bytes.length)(listener.codec)
          listener.evt.fire(decoded)
        buffer = buffer.removed(name)
      case other =>
        println(s"wanted to unbuffer $name, but state was ${other}")
        ()
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

    currentValues = currentValues.updated(name, Valued(reactive, summon))

    val initial = encode(name, reactive.now.state)
    channels.foreach(c => c.send(initial).run(println))

    reactive.observe: db =>
      println(s"publishing changes of $name to $channels")
      db.deltaBuffer.foreach: delta =>
        println(s"change is $delta")
        val msg = encode(name, delta)
        channels.foreach(c => c.send(msg).run(println))

  }
}
