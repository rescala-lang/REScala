import webview.WebView

import java.net.URI
import java.nio.file.{Files, Path}
import scala.concurrent.Future

// based on
// https://github.com/lolgab/webview-scala

object Main {
  def main(args: Array[String]): Unit = {

    val html = args.toList.headOption match
      case None       => raw"""<h1 id="eins"> HALLO WELT </h1>"""
      case Some(path) => Files.readString(Path.of(path))

    val w = WebView()
    // w.setHtml(html)
    w.navigate(Path.of(args.head).toUri)
    //callbackTests(w)
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
      "outpt",
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
