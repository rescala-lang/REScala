package todo

import org.scalajs.dom.{document, window}

import scala.scalajs.js.annotation.JSExportTopLevel

object Todolist {

  @JSExportTopLevel("Todolist")
  def run(): Unit = main(Array.empty[String])

  def main(args: Array[String]): Unit = {

    val storagePrefix = window.location.href
    println(storagePrefix)

    val todoApp = new TodoAppUI(storagePrefix)
    val div     = todoApp.getContents()

    document.body.replaceChild(div.render, document.body.firstElementChild)
    ()
  }

}
