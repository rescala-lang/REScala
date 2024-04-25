package calendar

import org.scalajs.dom.{document, window}
import rdts.base.Uid

import scala.scalajs.js.annotation.JSExportTopLevel
import channel.webrtc.Example
import scalatags.JsDom.all.*

object Calendar {

  given replicaId: Uid = Uid.gen()

  @JSExportTopLevel("Calendar")
  def run(): Unit = main(Array.empty[String])

  def main(args: Array[String]): Unit = {

    val storagePrefix = window.location.href
    println(storagePrefix)

    val calendar = new CalendarUI(storagePrefix, replicaId)
    val div      = calendar.getContents()

    val webrtc = Example.example()

    document.body.replaceChild(div, document.body.firstElementChild)
    document.body.appendChild(webrtc.render)
    ()
  }

}
