package daimpl.todo

import org.scalajs.dom
import org.scalajs.dom.html.Input
import org.scalajs.dom.{UIEvent, document}
import rescala.core.ReSerializable
import rescala.restoration.LocalStorageStore

import scala.scalajs.js.annotation.JSExportTopLevel
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section

import rescalatags._

object TodoMVC {

  implicit val storingEngine: LocalStorageStore = new LocalStorageStore()
  import storingEngine._
  import rescala.restoration.ReCirce._

//  var unique = 0

  implicit val taskDecoder: io.circe.Decoder[Task] = io.circe.Decoder.forProduct3[String, Boolean, List[String], Task]("decs", "done", "names") { (dec, don, names) =>
    storingEngine.addNextNames(names: _*)
    new Task(dec, don)
  }
  implicit val taskEncoder: io.circe.Encoder[Task] = io.circe.Encoder.forProduct3[String, Boolean, List[String], Task]("decs", "done", "names"){t =>
    (t.desc.readValueOnce, t.done.readValueOnce, List(getName(t.desc), getName(t.done)))
  }

  class Task(desc_ : String, done_ : Boolean) {
//    val id   = unique
    val desc = Var(desc_)
    val done = Var(done_)
    val editing = Var(false)(ReSerializable.doNotSerialize, implicitly)
//    unique += 1
  }

  @JSExportTopLevel("daimpl.todo.TodoMVC.main")
  def main(): Unit = {

    val innerTasks = List(
      new Task("get milk", false),
      new Task("get sugar", false),
      new Task("get coffee", false),
      new Task("walk the dog", false)
    )
    storingEngine.addNextNames("tasklist")
    val tasks = Var(innerTasks)

    lazy val newTodo: Input = input(
      id := "newtodo",
      `class` := "new-todo",
      placeholder := "What needs to be done?",
      autofocus := "",

      // TODO onchange --> on enter
      onchange := { e: UIEvent =>
        e.preventDefault()
        tasks.transform(new Task(newTodo.value, false) :: _)
        newTodo.value = ""
      }
    ).render

    document.body.replaceChild(div(
      `class`:="todoapp",
      header(
        `class`:="header",
        h1("todos"),
        newTodo
      ),

      section(
        `class`:= "main",
        `style`:= Signal { if(tasks().isEmpty) "display:hidden" else "" },
        input( `class`:="toggle-all", `type`:="checkbox",
//          checked:=tasks.now.map { it => it.done.now }
//                   .reduce {  (a, b) => a && b},
          onclick:={ e: dom.UIEvent =>
            println("eh?")
            tasks.readValueOnce.foreach { it =>
              it.done set !e.target.asInstanceOf[dom.html.Input].checked } }),
        label(`for`:="toggle-all", "Mark all as complete"),
        Signal.dynamic { ul(`class`:="todo-list", tasks().map { t =>

          li(
            `class`:=
               (if (t.done()) "completed " else "")
              +(if (t.editing()) "editing " else ""),

            div(
              `class`:="view",

              ondblclick:={ e: dom.UIEvent => t.editing set true },

              input( `class`:="toggle", `type`:="checkbox",
                if (t.done()) checked else "",
                onchange:={ e: dom.UIEvent =>
                  t.done set e.target.asInstanceOf[dom.html.Input].checked }
              ),

              label(t.desc()),

              button(`class`:="destroy",
                onclick:= { e: dom.UIEvent =>
                  tasks.transform(_.filter { it => it != t })
                })
            ),

            input( `class`:="edit", `type`:="text",
              value:=t.desc(),
              onchange:={ e: dom.UIEvent =>
                val input = e.target.asInstanceOf[dom.html.Input]
                t.editing set false
                t.desc set input.value
              }
            )
          )

        }) }.asFrag
      ),

//            onchange := { e: dom.UIEvent =>
//              t.desc set e.target.asInstanceOf[dom.html.Input].value
//              tasks set tasks.now.filter { (x)=> x.desc.now != "" }
//            }

      input(
        `type`:="button",
        `class`:=Signal { if (tasks().size==0) "hidden" else ""},
        value:="remove all done todos", onclick:={ e: dom.UIEvent =>
          tasks set tasks.readValueOnce.filter { t => !t.done.readValueOnce }
        }
      )
    ).render, document.body.firstElementChild)
  }
}
