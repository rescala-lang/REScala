package channels

import de.rmgk.delay.Callback

import java.io.IOException
import java.nio.channels.ClosedChannelException
import scala.util.{Failure, Success}

object TestUtil {

  def printErrors[T](cb: T => Unit): Callback[T] =
    case Success(mb) => cb(mb)
    case Failure(ex) => ex match
        case ex: IOException if ex.getCause.isInstanceOf[InterruptedException] =>
        case ex: ClosedChannelException                                        =>
        case ex                                                                => ex.printStackTrace()
}
