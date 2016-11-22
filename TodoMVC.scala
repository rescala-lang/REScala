package daimpl.todomvc

import scala.scalajs.js.JSApp
import scalatags.JsDom.all._
import rescala._
import rescalatags._
import org.scalajs.dom
import dom.document

object TodoMVC extends JSApp {
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

    val section = div // TODO

    document.body.replaceChild(div(
      `class`:="todoapp",
      header(
        `class`:="header",
        h1("todos"),
        input(
          id:="newtodo",
          `class`:="new-todo",
          placeholder:="What needs to be done?",
          autofocus:="",

          // TODO onchange --> on enter
          onchange:= { e: dom.UIEvent =>
            e.preventDefault()
            val input = document.getElementById("newtodo")
              .asInstanceOf[dom.html.Input]
            tasks() = new Task(input.value, false) :: tasks.now
            input.value = ""
          }

        )
      ),

      section(
        `class`:="main",
        `style`:=Signal { if(tasks().size==0) "display:hidden" else "" },
        input(`class`:="toggle-all", `type`:="checkbox"),
        label(`for`:="toggle-all", "Mark all as complete"),
        Signal { ul(`class`:="todo-list",tasks().map { t =>

          // completed
          li(
            if (t.done()) `class`:="completed" else "",
            div(
              `class`:="view",
              input(
                `class`:="toggle",
                `type`:="checkbox",
                if (t.done()) checked else "",
                onchange:={ e: dom.UIEvent =>
                  t.done() = e.target.asInstanceOf[dom.html.Input].checked
                }
              ),
              label(t.desc()),
              button(`class`:="destroy")
            ),
            input(`class`:="edit", value:="Create a TodoMVC template")
          )
          
        }) }.asFrag
      ),

// TODO implement todo editing
//          // bidirectional binding only with onchange, not with oninput :(
//            onchange := { e: dom.UIEvent =>
//              t.desc() = e.target.asInstanceOf[dom.html.Input].value
//              tasks() = tasks.now.filter { (x)=> x.desc.now != "" }
//            }

      input(
        `type`:="button",
        `class`:=Signal { if (tasks().size==0) "hidden" else ""},
        value:="remove all done todos", onclick:={ e: dom.UIEvent =>
          tasks() = tasks.now.filter { t => !t.done.now }
        }
      )
    ).render, document.body.firstElementChild)
  }
}
