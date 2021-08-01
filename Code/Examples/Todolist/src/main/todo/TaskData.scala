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
import rescala.extra.lattices.delta.crdt.reactive.LWWRegister
import rescala.extra.lattices.delta.Codecs._
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._

import java.util.concurrent.ThreadLocalRandom
import scala.Function.const
import scala.scalajs.js.timers.setTimeout

case class TaskData(
    desc: String,
    done: Boolean = false
) {
  def toggle(): TaskData          = copy(done = !done)
  def edit(str: String): TaskData = copy(desc = str)
}

object TaskData {
  implicit val todoTaskCodec: JsonValueCodec[TaskData] = JsonCodecMaker.make
}

final class TaskRef(
    val task: Signal[LWWRegister[TaskData, DietMapCContext]],
    val tag: TypedTag[LI],
    val removed: Event[String],
    val id: String,
) {
  override def hashCode(): Int = id.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case other: TaskRef => id == other.id
    case _              => false
  }
}

object TaskRef {
  implicit val transmittableLWW: IdenticallyTransmittable[LWWRegister.State[TaskData, DietMapCContext]] =
    IdenticallyTransmittable()

  def signalAndUI(
      replicaID: String,
      task: Option[TaskData],
      toggleAll: Event[UIEvent],
  ): TaskRef = {
    val lwwInit = LWWRegister[TaskData, DietMapCContext](replicaID)

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

    val deltaEvt = Evt[Delta[LWWRegister.State[TaskData, DietMapCContext]]]()

    val crdt = Events.foldAll(lww)(current =>
      Seq(
        doneEv act { _ => current.resetDeltaBuffer().map(_.toggle()) },
        edittextStr act { v => current.resetDeltaBuffer().map(_.edit(v)) },
        deltaEvt act { delta => current.resetDeltaBuffer().applyDelta(delta) }
      )
    )

    val taskID = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"

    LociDist.distributeDeltaCRDT(crdt, deltaEvt, Todolist.registry)(
      Binding[LWWRegister.State[TaskData, DietMapCContext] => Unit](taskID)
    )

    val taskData = crdt.map(_.read.getOrElse(TaskData(desc = "LWW Empty")))

    val removeButton =
      Events.fromCallback[UIEvent](cb => button(`class` := "destroy", onclick := cb))

    val editInput = edittext.value(value := taskData.map(_.desc)).render
    editDiv.event.observe(_ => setTimeout(0) { editInput.focus() })

    val listItem = li(
      `class` := editingV.map(if (_) "editing" else "no-editing"),
      editDiv.value(
        input(
          `class` := "toggle",
          `type`  := "checkbox",
          doneClick.value,
          checked := taskData.map(c => if (c.done) Some(checked.v) else None)
        ),
        label(taskData.map(c => stringFrag(c.desc)).asModifier),
        removeButton.value
      ),
      editInput
    )

    new TaskRef(crdt, listItem, removeButton.event.map(_ => taskID), taskID)
  }
}
