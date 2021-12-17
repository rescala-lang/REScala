package todo

import loci.registry.Binding
import loci.serializer.jsoniterScala.jsoniteScalaBasedSerializable
import loci.transmitter.IdenticallyTransmittable
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Input, LI}
import rescala.default._
import rescala.extra.Tags._
import rescala.extra.distributables.LociDist
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.Codecs._
import rescala.extra.lattices.delta.crdt.reactive.LWWRegister
import rescala.extra.lattices.delta.interfaces.MVRegisterInterface
import rescala.extra.lattices.delta.{Delta, TimedVal}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import todo.Todolist.replicaId

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

case class TaskRef(id: String) {
  lazy val cached: TaskRefData = TaskRefs.lookupOrCreateTaskRef(id, None)

  def task: Signal[LWWRegister[TaskData, DietMapCContext]] = cached.task
  def tag: TypedTag[LI]                                    = cached.tag
  def removed: Event[String]                               = cached.removed
}

final class TaskRefData(
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

object TaskRefs {
  private val taskRefMap: mutable.Map[String, TaskRefData] = mutable.Map.empty

  var taskrefObj: TaskRefObj = null

  def lookupOrCreateTaskRef(id: String, task: Option[TaskData]): TaskRefData = {
    TaskRefs.taskRefMap.getOrElseUpdate(id, { taskrefObj.createTaskRef(id, task) })
  }
}

class TaskRefObj(toggleAll: Event[UIEvent], storePrefix: String) {

  import Codecs.todoTaskCodec

  implicit val transmittableLWW: IdenticallyTransmittable[LWWRegister.State[TaskData, DietMapCContext]] =
    IdenticallyTransmittable()

  def createTaskRef(
      taskID: String,
      task: Option[TaskData],
  ): TaskRefData = {
    val lwwInit = LWWRegister[TaskData, DietMapCContext](replicaId)

    val lww = task match {
      case None => lwwInit.mutate((replicaID, state) =>
          MVRegisterInterface.write[TimedVal[TaskData], DietMapCContext](
            TimedVal(TaskData("<empty>"), replicaID, 0, 0)
          ).apply(replicaID, state)
        )
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

    // type Carrier = LWWRegister.State[TaskData, DietMapCContext]
    //
    // val merge = implicitly[UIJDLattice[Carrier]]
    //
    // val crdtAlt = DeltaStateReactive.create[Carrier, Carrier](
    //  lww,
    //  deltaEvt,
    //  (s, d) => merge.merge(s, d),
    //  Seq(
    //    { (dt: DynamicTicket, current: Carrier) => dt.depend(doneEv); current },
    //  )
    // )

    val crdt = Storing.storedAs(storePrefix + taskID, lww) { init =>
      Events.foldAll(init)(current =>
        Seq(
          doneEv act { _ => current.resetDeltaBuffer().map(_.toggle()) },
          edittextStr act { v => current.resetDeltaBuffer().map(_.edit(v)) },
          deltaEvt act { delta => current.resetDeltaBuffer().applyDelta(delta) }
        )
      )
    }(Codecs.codecLww)

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

    new TaskRefData(crdt, listItem, removeButton.event.map(_ => taskID), taskID)
  }
}
