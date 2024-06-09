package todo

import org.scalajs.dom.{document, window}
import rdts.base.Uid
import replication.Storing
import replication.JsoniterCodecs.given

import scala.scalajs.js.annotation.JSExportTopLevel

object Todolist {

  val replicaId: Uid = Storing.initializedOnce("replicaId", Uid.gen())

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
