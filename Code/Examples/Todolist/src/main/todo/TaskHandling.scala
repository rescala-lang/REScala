package src.main.todo

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import loci.registry.Binding
import loci.transmitter.transmittable.IdenticallyTransmittable
import loci.serializer.jsoniterScala.jsoniteScalaBasedSerializable
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Input, LI}
import rescala.default._
import rescala.extra.Tags._
import rescala.extra.distributables.LociDist
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.Delta
import rescala.extra.lattices.delta.impl.reactive.LastWriterWins
import rescala.extra.lattices.delta.Codecs._
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

import java.util.concurrent.ThreadLocalRandom
import scala.Function.const
import scala.scalajs.js.timers.setTimeout

case class TodoTask(
    id: String = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})",
    desc: String,
    done: Boolean = false
) {
  def toggle(): TodoTask          = copy(done = !done)
  def edit(str: String): TodoTask = copy(desc = str)
}

object TodoTask {
  implicit val todoTaskCodec: JsonValueCodec[TodoTask] = JsonCodecMaker.make
}

case class TodoTaskView(tag: TypedTag[LI], removeEvt: Event[String], writeEvt: Event[TodoTask])

object TodoTaskView {
  implicit val transmittableLWW: IdenticallyTransmittable[LastWriterWins.State[TodoTask, DietMapCContext]] =
    IdenticallyTransmittable()

  def signalAndUI(
      replicaID: String,
      taskID: String,
      task: Option[TodoTask],
      toggleAll: Event[UIEvent]
  ): (Signal[LastWriterWins[TodoTask, DietMapCContext]], TypedTag[LI], Event[String]) = {
    val lwwInit = LastWriterWins[TodoTask, DietMapCContext](replicaID)

    val lww = task match {
      case None    => lwwInit
      case Some(v) => lwwInit.write(v)
    }

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

    val doneEv = toggleAll || doneClick.event

    val deltaEvt = Evt[Delta[LastWriterWins.State[TodoTask, DietMapCContext]]]()

    val crdt = Events.foldAll(lww)(current =>
      Seq(
        doneEv act { _ => current.resetDeltaBuffer().map(_.toggle()) },
        edittextStr act { v => current.resetDeltaBuffer().map(_.edit(v)) },
        deltaEvt act { delta => current.resetDeltaBuffer().applyDelta(delta) }
      )
    )

    LociDist.distributeDeltaCRDT(crdt, deltaEvt, Todolist.registry)(
      Binding[LastWriterWins.State[TodoTask, DietMapCContext] => Unit](taskID)
    )

    val taskData = crdt.map(_.read.getOrElse(TodoTask(desc = "LWW Empty")))

    val removeButton =
      Events.fromCallback[UIEvent](cb => button(`class` := "destroy", onclick := cb))

    val editInput = edittext.value(value := taskData.map(_.desc)).render
    editDiv.event.observe(_ => setTimeout(0) { editInput.focus() })

    val listItem = li(
      `class` := editingV.map(if (_) "editing" else "no-editing"),
      editDiv.value(
        input(
          `class` := "toggle",
          `type` := "checkbox",
          doneClick.value,
          checked := taskData.map(c => if (c.done) Some(checked.v) else None)
        ),
        label(taskData.map(c => stringFrag(c.desc)).asModifier),
        removeButton.value
      ),
      editInput
    )

    (crdt, listItem, removeButton.event.map(_ => taskID))
  }

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

    val removeButton =
      Events.fromCallback[UIEvent](cb => button(`class` := "destroy", onclick := cb))

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
