import java.io._
import java.net.{ InetAddress, ServerSocket, Socket, SocketException }
import java.util.Random
import reshapes.figures._
import java.awt.Point
import reshapes.util.MathUtil

/**
 * Simple client/server application using Java sockets.
 *
 * The server simply generates random integer values and
 * the clients provide a filter function to the server
 * to get only values they interested in (eg. even or
 * odd values, and so on).
 */
object randomclient {

  def main(args: Array[String]) {
    val line = new Rectangle()
    line.update(List(new Point(0, 0), new Point(1, 1), new Point(2, 2), new Point(4, 4)))

    println(MathUtil.getIntersectionsOfTwoLines((1, 1, 3, 3), (1, 3, 2, 1)))
  }
}
