package src.main.todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import loci.registry.Binding
import loci.serializer.jsoniterScala.jsoniteScalaBasedSerializable
import loci.transmitter.transmittable.IdenticallyTransmittable
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Input, LI}
import rescala.default._
import rescala.extra.Tags._
import rescala.extra.distributables.LociDist
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.Codecs._
import rescala.extra.lattices.delta.Delta
import rescala.extra.lattices.delta.crdt.reactive.LWWRegister
import scalatags.JsDom.TypedTag
import src.main.todo.Todolist.replicaId
import scalatags.JsDom.all._

import scala.Function.const
import scala.collection.mutable
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

class TaskRefObj(toggleAll: Event[UIEvent]) {

  private val taskRefMap: mutable.Map[String, TaskRef] = mutable.Map.empty

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = new JsonValueCodec[TaskRef] {
    override def decodeValue(in: JsonReader, default: TaskRef): TaskRef =
      lookupOrCreateTaskRef(in.readString(default.id), None)
    override def encodeValue(x: TaskRef, out: JsonWriter): Unit = out.writeVal(x.id)
    override def nullValue: TaskRef                             = new TaskRef(Var.empty, li(), Evt(), "")
  }

  implicit val transmittableLWW: IdenticallyTransmittable[LWWRegister.State[TaskData, DietMapCContext]] =
    IdenticallyTransmittable()

  def lookupOrCreateTaskRef(id: String, task: Option[TaskData]): TaskRef = {
    taskRefMap.getOrElseUpdate(id, { createTaskRef(id, task) })
  }

  def createTaskRef(
      taskID: String,
      task: Option[TaskData],
  ): TaskRef = {
    val lwwInit = LWWRegister[TaskData, DietMapCContext](replicaId)

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
    val editingV      = changeEditing.latest(init = false)

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
