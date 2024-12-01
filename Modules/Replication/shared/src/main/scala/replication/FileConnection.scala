package replication

import channels.LatentConnection
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, scanJsonValuesFromStream, writeToString}
import de.rmgk.delay.{Async, Callback, Sync}

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.Using

class FileConnection[T](path: Path)(using JsonValueCodec[ProtocolMessage[T]])
    extends LatentConnection[ProtocolMessage[T]] {

  class InnerConnection(peerFun: => Callback[ProtocolMessage[T]]) extends channels.Connection[ProtocolMessage[T]] {
    lazy val peer = peerFun
    def send(message: ProtocolMessage[T]): Async[Any, Unit] = Async.fromCallback {
      message match
        case ProtocolMessage.Request(sender, knows) =>
          Using(Files.newInputStream(path)) { is =>
            scanJsonValuesFromStream[ProtocolMessage[T]](is) {
              case pm @ ProtocolMessage.Payload(_, dots, _) if !(dots <= knows) =>
                peer.succeed(pm)
                true
              case _ => true
            }
          }
          ()
        case pl: ProtocolMessage.Payload[T] =>
          val res = writeToString[ProtocolMessage[T]](pl)
          Files.writeString(path, res + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND)
          ()
        case ProtocolMessage.Ping(time) => peer.succeed(ProtocolMessage.Pong(time))
        case ProtocolMessage.Pong(time) => ()
    }
    def close(): Unit = ()
  }

  def prepare(receiver: channels.Receive[ProtocolMessage[T]])
      : de.rmgk.delay.Async[channels.Abort, channels.Connection[ProtocolMessage[T]]] =
    Sync {
      lazy val connection: InnerConnection = InnerConnection(receiver.messageHandler(connection))
      connection
    }
}
