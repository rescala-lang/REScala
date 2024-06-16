package calendar

import org.scalajs.dom.{document, window}
import rdts.base.Uid

import scala.scalajs.js.annotation.JSExportTopLevel

object Calendar {

  given replicaId: Uid = Uid.gen()

  @JSExportTopLevel("Calendar")
  def run(): Unit = main(Array.empty[String])

  def main(args: Array[String]): Unit = {

    val storagePrefix = window.location.href
    println(storagePrefix)

    val calendar = new CalendarUI(storagePrefix, replicaId)
    val div      = calendar.getContents()


    document.body.replaceChild(div, document.body.firstElementChild)
    ()
  }

}
