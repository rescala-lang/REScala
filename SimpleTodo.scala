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
        `class`:="task",
        onsubmit:= { e: dom.UIEvent =>
          e.preventDefault()

          val value = document.getElementById("newtodo")
            .asInstanceOf[dom.html.Input].value

          tasks() = new Task(value, false) :: tasks.now
        },
        span(`class`:="span-input"),
        input(`class`:="descrip", id:="newtodo", placeholder:="new todo")
      ),

      Signal { ul(tasks().map { t =>
        li(
          `class`:=Signal{ if (t.done()) "task done" else "task" },
          Signal { input(
            `type`:="checkbox",

            // TODO ? use attrValue / .asAttr
            if (t.done()) checked:="checked" else "",

            onchange:={ e: dom.UIEvent =>
              t.done() = e.target.asInstanceOf[dom.html.Input].checked
            }
          ) }.asFrag,

          // bidirectional binding only with onchange, not with oninput :(
          input(
            value := t.desc(),
            onchange := { e: dom.UIEvent =>
              t.desc() = e.target.asInstanceOf[dom.html.Input].value
              tasks() = tasks.now.filter { (x)=> x.desc.now != "" }
            }
          )
        )
      }) }.asFrag,

      input(`type`:="button", value:="remove all done todos", onclick:={ e: dom.UIEvent =>
        tasks() = tasks.now.filter { t => !t.done.now }
      })
    ).render)
  }
}
