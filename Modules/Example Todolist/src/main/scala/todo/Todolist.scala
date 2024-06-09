package todo

import org.scalajs.dom.{document, window}
import rdts.base.Uid

import scala.scalajs.js.annotation.JSExportTopLevel

object Todolist {

  val replicaId: Uid = Uid.gen()

  @JSExportTopLevel("Todolist")
  def run(): Unit = main(Array.empty[String])

  def main(args: Array[String]): Unit = {

    val storagePrefix = window.location.href
    println(storagePrefix)

    val todoApp = new TodoAppUI(storagePrefix)
    val div     = todoApp.getContents()

    val webrtc = Example.example()

    document.body.replaceChild(div, document.body.firstElementChild)
    document.body.appendChild(webrtc.render)
    ()
  }

}
