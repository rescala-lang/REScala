package channels

import de.rmgk.delay.Callback

import java.io.IOException
import java.net.{InetAddress, InetSocketAddress, ServerSocket, SocketException}
import java.nio.channels.ClosedChannelException
import java.util.concurrent.{Executors, Semaphore}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object TestUtil {

  def printErrors[T](cb: T => Unit): Callback[T] =
    case Success(mb) => cb(mb)
    case Failure(ex) => ex match
        case ex: IOException if ex.getCause.isInstanceOf[InterruptedException] =>
        case ex: ClosedChannelException                                        =>
        case ex                                                                => ex.printStackTrace()
}
