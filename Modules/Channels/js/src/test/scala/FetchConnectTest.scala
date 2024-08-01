import channels.{Abort, JSHttpPseudoChannel}
import rdts.base.LocalUid

import scala.util.{Failure, Success}

object FetchConnectTest {

  def main(args: Array[String]): Unit = {
    JSHttpPseudoChannel.connect(s"http://localhost:58080/channel", LocalUid.gen()).prepare { conn =>
      {
        case Success(msg) => println(msg.convert: String)
        case Failure(ex)  => ex.printStackTrace
      }
    }.run(using Abort()) {
      case Success(conn) =>
      case Failure(ex)   => ex.printStackTrace()
    }
  }

}
