package channels

import de.rmgk.delay.{Async, Sync}

import java.nio.file.{Files, Path, StandardOpenOption}

object FileOutput {

  class FileOutputConnection(path: Path) extends ConnectionContext {
    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      Files.write(path, message.asArray, StandardOpenOption.APPEND)
      ()
    }
    override def close(): Unit = ()
  }

  def to(path: Path): LatentConnection = new LatentConnection {
    def prepare(incoming: Incoming): Async[Abort, ConnectionContext] = Sync {
      val foc = FileOutputConnection(path)
      incoming(foc)
      foc
    }
  }

}
