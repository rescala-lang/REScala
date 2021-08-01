package src.main.todo

import loci.registry.Registry
import org.scalajs.dom.document

object Todolist {

  val todoApp = new TodoAppUI()

  val registry = new Registry

  def main(args: Array[String]): Unit = {

    val div = todoApp.getContents()

    val webrtc = WebRTCHandling(registry)

    document.body.replaceChild(div.render, document.body.firstElementChild)
    document.body.appendChild(webrtc.webrtcHandlingArea.render)

  }


}
