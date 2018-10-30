package todo

import java.util.concurrent.ThreadLocalRandom

import org.scalajs.dom
import org.scalajs.dom.html.Input
import org.scalajs.dom.{UIEvent, document}
import rescala.core.ReSerializable
import rescala.restoration.{LocalStorageStore, ReCirce}
import rescala.restoration.ReCirce.{recirce, varDecoder, varEncoder}
import rescala.debuggable.ChromeDebuggerInterface
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
      val rn = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"
      val descV = Var(desc)(implicitly, rn.toString)
      val doneV = Var(done)(implicitly, rn.toString + "b")
      storingEngine.registerSource(descV)
      storingEngine.registerSource(doneV)
      new Task(descV, doneV)
    }
  }

  @JSExportTopLevel("todo.TodoMVC.main")
  def main(): Unit = {
    ChromeDebuggerInterface.setup(storingEngine)

    val innerTasks = List(
      Task("walk the dog", false),
      Task("get milk", false),
      Task("get coffee", false)
    )
    val tasks = Var(innerTasks)(ReCirce.recirce, "tasklist")

    lazy val newTodo: Input = input(
      id := "newtodo",
      `class` := "new-todo",
      placeholder := "What needs to be done?",
      autofocus := "autofocus",

      onchange := { e: UIEvent =>
        e.preventDefault()
        if (newTodo.value.trim != "")
          tasks.transform(Task(newTodo.value.trim, done = false) :: _)
        newTodo.value = ""
      }
    ).render

    val content = div(
      `class`:="todoapp",
      header(
        `class`:="header",
        h1("todos"),
        newTodo
      ),

      section(
        `class`:= "main",
        `style`:= Signal { if(tasks().isEmpty) "display:hidden" else "" },
        input( id:="toggle-all", name:="toggle-all", `class`:="toggle-all", `type`:="checkbox",
          onchange:={ e: dom.UIEvent =>
            tasks.readValueOnce.foreach { it =>
              it.done set e.target.asInstanceOf[dom.html.Input].checked } }),
        label(`for`:="toggle-all", "Mark all as complete"),
        Signal.dynamic { ul(`class`:="todo-list", tasks().map { t =>

          val change = { e: dom.UIEvent =>
            val myinput = e.target.asInstanceOf[dom.html.Input]
            t.desc set myinput.value.trim
            t.editing set false
            tasks.transform(_.filter { t => t.desc.readValueOnce != "" }) }

          val myinput = input(
            `class`:="edit", `type`:="text",
            value:=t.desc(),
            onchange:=change, onblur:=change,
            onkeypress:={e: dom.KeyboardEvent => if (e.keyCode == 13) { // 13 = enter key
              e.preventDefault() // TODO somehow app breaks, if we listen to enter...?
            }},
            ).render

          li(
            `class`:=
               (if (t.done()) "completed " else " ")
              +(if (t.editing()) "editing " else "no-editing "),

            div(
              `class`:="view",

              ondblclick:={ e: dom.UIEvent =>
                tasks.readValueOnce.foreach( tt => tt.editing.set(t==tt) )
              },

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

            myinput
          )
        }) }.asFrag
      ),

      div(
        `class`:="footer",
        `style`:= Signal { if(tasks().isEmpty) "display:none" else "" },

        Signal.dynamic {
          val leftTasks = tasks().filter(!_.done())
          span(
            `class`:="todo-count",
            strong("" + leftTasks.size),
            span(if (leftTasks.size == 1)
              " item left" else " items left")
          )
        }.asFrag,

        button(
          `class`:=Signal.dynamic {
            "clear-completed" +
            (if (tasks().filter(t => t.done()).size==0)
              " hidden" else "") },
          onclick:={ e: dom.UIEvent =>
            tasks.transform(_.filter { t => !t.done.readValueOnce }) },
          "remove all done todos"
        )
      )
    )

    document.body.replaceChild(content.render, document.body.firstElementChild)

    ChromeDebuggerInterface.finishedLoading()
  }
}
