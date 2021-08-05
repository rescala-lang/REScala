package todo

import loci.registry.Registry
import org.scalajs.dom.document

import java.util.concurrent.ThreadLocalRandom

object Todolist {

  val replicaId: String = ThreadLocalRandom.current().nextLong().toHexString

  val todoApp = new TodoAppUI()

  val registry = new Registry

  def main(args: Array[String]): Unit = {

    val div = todoApp.getContents()

    val webrtc = WebRTCHandling(registry)

    document.body.replaceChild(div.render, document.body.firstElementChild)
    document.body.appendChild(webrtc.webrtcHandlingArea.render)

  }


}
