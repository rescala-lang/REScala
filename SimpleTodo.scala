package tutorial.webapp

import scala.scalajs.js.JSApp
import scalatags.JsDom.all._
import rescala._
import rescalatags._
import org.scalajs.dom
import dom.document

object SimpleTodo extends JSApp {
  var unique = 0

  class Task(desc_ : String, done_ : Boolean) {
    val id   = unique
    val desc = Var(desc_)
    val done = Var(done_)
    unique += 1
  }

  def main(): Unit = {
    val tasks = Var(List(
      new Task("get milk", false),
      new Task("get sugar", false),
      new Task("get coffee", false),
      new Task("walk the dog", false)
    ))
    document.body.appendChild(div(
      h1("TODO!"),
      form(
        action:="#",
        onsubmit:= { (e: dom.UIEvent) =>
          val value = e.target.asInstanceOf[dom.html.Form].
            children(0).asInstanceOf[dom.html.Input].value
          tasks() = new Task(value, false) :: tasks.now
          e.preventDefault()
        },
        input(`class`:="descrip", name:="newtodo", placeholder:="new todo")
      ),
      Signal { ul(tasks().map {
        (t) => {
          li(`class` := "task",
            input(
              value := t.desc(),
              onchange := { (e: dom.UIEvent) =>
                t.desc() = e.target.asInstanceOf[dom.html.Input].value
                tasks() = tasks.now.filter { (x)=> x.desc.now != "" }
              }
            )
          )
        }
      }) }.asFrag
    ).render)
  }
}
