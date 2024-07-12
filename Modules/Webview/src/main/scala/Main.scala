import channels.{Abort, ArrayMessageBuffer, ConnectionContext, Incoming, LatentConnection, MessageBuffer}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import de.rmgk.delay.{Async, Sync}
import rdts.base.LocalUid
import replication.DataManager
import webview.WebView

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.concurrent.Future

object Main {
  def main(args: Array[String]): Unit = {

    if args.isEmpty then
      println(s"requires a path to the html as a first argument")
      return

    def receiveCallback(state: TodoRepState): Unit = {
      println(s"executing receive callback")
      println(state)
      try
        Files.write(
          Path.of("webviewoutputtest.json"),
          writeToArray(state),
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        )
        Files.write(
          Path.of("webviewoutputtest.json"),
          "\n".getBytes,
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        )
      catch
        case ex: Exception => ex.printStackTrace()
      ()
    }

    val dataManager = DataManager[TodoRepState](LocalUid.gen(), receiveCallback, _ => ())

    val w = WebView()
    dataManager.addLatentConnection(WebviewNativeChannel.listen(w))
    w.navigate(Path.of(args.head).toUri)

    w.run()
  }

  private def callbackTests(w: WebView) = {
    w.init(
      raw"""
          function callback() {
            const elem = document.getElementById("eins");
            elem.textContent = 10
            outpt(1)
          }
          setInterval("callback()",1000);
    """
    )
    val res = w.bind(
      "webview_channel_send",
      b => {
        println("received:")
        println(b)
        b
      }
    )
    Future {
      Thread.sleep(1000)
      w.setHtml("hhohoho")
    }(using scala.concurrent.ExecutionContext.global)
  }
}

object WebviewNativeChannel {

  given JsonValueCodec[List[String]] = JsonCodecMaker.make

  class WebviewConnectionContext(w: WebView) extends ConnectionContext {
    override def send(message: MessageBuffer): Async[Any, Unit] = Sync {
      val b64 = new String(java.util.Base64.getEncoder.encode(message.asArray))
      w.eval(s"""webview_channel_receive('$b64')""")
    }
    override def close(): Unit = ()
  }

  def listen(w: WebView): LatentConnection = new LatentConnection {
    def prepare(incoming: Incoming): Async[Abort, ConnectionContext] = Sync {
      val conn = WebviewConnectionContext(w)
      val cb   = incoming(conn)
      w.bind(
        "webview_channel_send",
        { msg =>
          println(s"received message webview -> native")
          val arguments = readFromString[List[String]](msg)
          val bytes     = java.util.Base64.getDecoder.decode(arguments.head)
          println(s"successfully decoded base 64")
          println(new String(bytes))
          cb.succeed(ArrayMessageBuffer(bytes))
          println(s"done")
          ""
        }
      )
      conn
    }
  }
}
