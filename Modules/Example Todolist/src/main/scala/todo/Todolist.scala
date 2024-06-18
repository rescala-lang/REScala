package todo

import org.scalajs.dom.{document, window}
import rdts.base.{Lattice, Uid}
import reactives.extra.Tags.reattach
import replication.WebRTCConnectionView
import scalatags.JsDom.all

import scala.scalajs.js.annotation.JSExportTopLevel

object Todolist {

  val replicaId: Uid = Uid.gen()

  @JSExportTopLevel("Todolist")
  def run(): Unit = main(Array.empty[String])

  def main(args: Array[String]): Unit = {

    val storagePrefix = window.location.href
    println(storagePrefix)

    val todoApp  = new TodoAppUI(storagePrefix)
    val contents = todoApp.getContents()

    val webrtc = WebRTCConnectionView(TodoDataManager.dataManager).example()

    document.body.replaceChild(contents, document.body.firstElementChild)

    document.body.appendChild(webrtc.render)

    document.body.appendChild:
      all.div.render.reattach(TodoDataManager.receivedCallback.map(_ =>
        val state = TodoDataManager.dataManager.allDeltas.reduceOption(Lattice.merge)
        all.pre(all.stringFrag(pprint.apply(state).plainText)).render
      ).hold(all.span.render))

    ()
  }

}
