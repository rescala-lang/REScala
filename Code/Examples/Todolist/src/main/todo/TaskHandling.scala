package todo

import io.circe.generic.semiauto
import io.circe.{Decoder, Encoder}
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Input, LI}
import rescala.default._
import rescala.extra.Tags._
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

import java.util.concurrent.ThreadLocalRandom
import scala.Function.const
import scala.scalajs.js.timers.setTimeout

case class TodoTask(id: String = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})", desc: String, done: Boolean = false) {
  def toggle(): TodoTask = copy(done = !done)
  def edit(str: String): TodoTask = copy(desc = str)
}

object TodoTask {
  implicit val todoTaskDecoder: Decoder[TodoTask] = semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val todoTaskEncoder: Encoder[TodoTask] = semiauto.deriveEncoder: @scala.annotation.nowarn
}

case class TodoTaskView(tag: TypedTag[LI], removeEvt: Event[String], writeEvt: Event[TodoTask])

object TodoTaskView {
  def fromTask(task: TodoTask): TodoTaskView = {
    val edittext = Events.fromCallback[UIEvent] { inputChange =>
      input(`class` := "edit", `type` := "text", onchange := inputChange, onblur := inputChange)
    }

    val edittextStr = edittext.event.map { e: UIEvent =>
      val myinput = e.target.asInstanceOf[Input]
      myinput.value.trim
    }

    val editDiv = Events.fromCallback[UIEvent] { cb =>
      div(`class` := "view", ondblclick := cb)
    }

    val changeEditing = (edittextStr map const(false)) || (editDiv.event map const(true))
    val editingV      = changeEditing.latest(init = false)(implicitly)

    val doneClick = Events.fromCallback[UIEvent](onchange := _)

    val writeEvt = doneClick.event.map(_ => task.toggle()) || edittextStr.map(str => task.edit(str))

    writeEvt observe { _ =>
      println(s"writeEvt was fired for task $task")
    }

    val removeButton =
      Events.fromCallback[UIEvent](cb => button(`class` := "destroy", onclick := cb))

    removeButton.event observe { _ =>
      println(s"removeEvt was fired for task $task")
    }

    val editInput = edittext.value(value := task.desc).render
    editDiv.event.observe(_ => setTimeout(0) { editInput.focus() })

    val listItem = li(
      `class` := editingV.map(if (_) "editing" else "no-editing"),
      editDiv.value(
        input(
          `class` := "toggle",
          `type` := "checkbox",
          doneClick.value,
          checked := (if (task.done) Some(checked.v) else None)
        ),
        label(stringFrag(task.desc)),
        removeButton.value
      ),
      editInput
    )

    TodoTaskView(listItem, removeButton.event.map(_ => task.id), writeEvt)
  }
}
