package daimpl.todo

import java.util.concurrent.ThreadLocalRandom

import org.scalajs.dom
import org.scalajs.dom.html.Input
import org.scalajs.dom.{UIEvent, document}
import rescala.core.ReSerializable
import rescala.restoration.{LocalStorageStore, ReCirce}
import rescala.restoration.ReCirce.{recirce, varDecoder, varEncoder}
import rescalatags._
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section

import scala.scalajs.js.annotation.JSExportTopLevel


object TodoMVC {

  implicit val storingEngine: LocalStorageStore = new LocalStorageStore()
  import storingEngine._

  implicit val taskDecoder: io.circe.Decoder[Task] = io.circe.Decoder.decodeTuple2[Var[String], Var[Boolean]].map { case (desc, done) =>
    new Task(desc, done)
  }
  implicit val taskEncoder: io.circe.Encoder[Task] = io.circe.Encoder.encodeTuple2[Var[String], Var[Boolean]].contramap{ t =>
    (t.desc, t.done)
  }

  class Task(val desc : Var[String], val done : Var[Boolean]) {
    val editing = Var(false)(ReSerializable.doNotSerialize, implicitly)
  }

  object Task {
    def apply(desc: String, done: Boolean) = {
      val rn = ThreadLocalRandom.current().nextLong()
        new Task(Var(desc)(implicitly, rn.toString), Var(done)(implicitly, rn.toString + "b"))
      }
  }

  @JSExportTopLevel("daimpl.todo.TodoMVC.main")
  def main(): Unit = {

    val innerTasks = List(
      Task("get milk", false),
      Task("get sugar", false),
      Task("get coffee", false),
      Task("walk the dog", false)
    )
    val tasks = Var(innerTasks)(ReCirce.recirce, "tasklist")

    lazy val newTodo: Input = input(
      id := "newtodo",
      `class` := "new-todo",
      placeholder := "What needs to be done?",
      autofocus := "",

      // TODO onchange --> on enter
      onchange := { e: UIEvent =>
        e.preventDefault()
        tasks.transform(Task(newTodo.value, false) :: _)
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
