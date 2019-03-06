package todo

import java.util.concurrent.ThreadLocalRandom

import io.circe.generic.semiauto
import io.circe.{Decoder, Encoder}
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Input, LI}
import rescala.Tags._
import rescala.core.ReSerializable
import rescala.restoration.LocalStorageStore
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import rescala.restoration.ReCirce._

import scala.Function.const
import scala.scalajs.js.timers.setTimeout

case class TaskData(desc: String, done: Boolean = false) {
  def toggle(): TaskData = copy(done = !done)
  def edit(str: String) = copy(desc = str)
}

object TaskData {
  implicit val taskDataDecoder: Decoder[TaskData] = semiauto.deriveDecoder
  implicit val taskDataEncoder: Encoder[TaskData] = semiauto.deriveEncoder
}




class TaskHandling(implicit val storingScheduler: LocalStorageStore) {
  import storingScheduler._

  class Taskref(val id: String,
                val listItem: TypedTag[LI],
                val contents: Signal[TaskData],
                val initial: TaskData,
                val removeClick: Event[String])

  object Taskref {
    implicit val taskDecoder: Decoder[Taskref] =
      Decoder.decodeTuple2[String, TaskData].map{ case (s, td) => maketask(td, s) }
    implicit val taskEncoder: Encoder[Taskref] =
      Encoder.encodeTuple2[String, TaskData].contramap[Taskref](tr => (tr.id, tr.initial))
  }


  val toggleAll = Events.fromCallback[UIEvent] { cb =>
    input(id := "toggle-all", name := "toggle-all", `class` := "toggle-all",
          `type` := "checkbox", onchange := cb)
  }




  def maketask(initial: TaskData,
               randomName: String = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})")
  : Taskref = {


    val edittext = Events.fromCallback[UIEvent]{ inputChange =>
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
    val editingV =  changeEditing.latest(init = false)(ReSerializable.doNotSerialize, implicitly)

    val doneClick = Events.fromCallback[UIEvent](onchange := _)

    val doneEv = toggleAll.event || doneClick.event

    val contents = Events.foldAll(initial)(current => Seq(
      doneEv >> const(current.toggle),
      edittextStr >> current.edit
    ))(implicitly[ReSerializable[TaskData]], randomName)


    val removeButton =
      Events.fromCallback[UIEvent](cb => button(`class` := "destroy", onclick := cb))

    val editInput = edittext.value(value := contents.map(_.desc)).render
    editDiv.event.observe(_ => setTimeout(0){editInput.focus()})

    val listItem = li(
      `class` := Signal(
        (if (contents.value.done) "completed " else "")
        + (if (editingV.value) "editing " else "no-editing ")
      ),

      editDiv.value(
        input(`class` := "toggle", `type` := "checkbox", doneClick.value,
              checked := contents.map(c => if (c.done) Some(checked.v) else None))(checked := false),
        label(contents.map(c => stringFrag(c.desc)).asModifier),
        removeButton.value
      ),
      editInput
    )

    new Taskref(randomName, listItem, contents, initial, removeButton.event.map(_ => randomName))
  }

}
