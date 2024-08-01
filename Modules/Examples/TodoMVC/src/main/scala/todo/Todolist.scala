package todo

import org.scalajs.dom.{document, window}
import rdts.base.{Lattice, LocalUid}
import reactives.extra.Tags.reattach
import dtn.rdt.Channel
import replication.WebRTCConnectionView
import scalatags.JsDom.all
import scalatags.JsDom.all.given
import todo.TodoDataManager.TodoRepState

import java.util.Timer
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object Todolist {

  val replicaId: LocalUid = LocalUid.gen()

  @JSExportTopLevel("Todolist")
  def run(): Unit = main(Array.empty[String])

  val timer = new Timer()

  def main(args: Array[String]): Unit = {

    val storagePrefix = window.location.href
    println(storagePrefix)

    val todoApp  = new TodoAppUI(storagePrefix)
    val contents = todoApp.getContents()

    TodoDataManager.dataManager.addLatentConnection(WebviewAdapterChannel.listen())

    val webrtc = WebRTCConnectionView(TodoDataManager.dataManager).example()

    document.body.replaceChild(contents, document.body.firstElementChild)

    document.body.appendChild(DTNTestConnector.getConnectorContents())

    document.body.appendChild(webrtc.render)

    document.body.appendChild:
      all.div.render.reattach(TodoDataManager.receivedCallback.map(_ =>
        val state = TodoDataManager.dataManager.allDeltas.reduceOption(Lattice.merge)
        all.pre(all.stringFrag(pprint.apply(state).plainText)).render
      ).hold(all.span.render))

    timer.scheduleAtFixedRate(
      { () =>
        TodoDataManager.dataManager.requestData()
      },
      1000,
      1000
    )

    ()
  }

}

object DTNTestConnector {
  def getConnectorContents() = {
    val portInput = all.input(all.placeholder := "dtnd ws port").render
    val connectButton = all.button(all.onclick := { () =>
      TodoDataManager.dataManager.addLatentConnection(
        Channel[TodoRepState]("127.0.0.1", portInput.value.toInt, "app1", scala.concurrent.ExecutionContext.global)
      )
    }).render
    connectButton.textContent = "Connect"

    all.div(portInput, connectButton).render
  }
}
