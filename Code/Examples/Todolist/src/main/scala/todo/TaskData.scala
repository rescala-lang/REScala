package todo

import kofre.datatypes.TimedVal
import kofre.decompose.interfaces.{CausalLastWriterWinsRegister, MultiVersionRegister}
import kofre.dotted.Dotted
import kofre.syntax.{DottedName, PermCausalMutate, PermId}
import loci.registry.Binding
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Input, LI}
import rescala.default.*
import rescala.extra.Tags.*
import rescala.extra.replication.{DeltaFor, ReplicationGroup}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.*
import todo.Todolist.replicaId
import Codecs.given
import kofre.deprecated.containers.DeltaBufferRDT
import kofre.primitives.MultiValueRegister
import loci.serializer.jsoniterScala.given

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
  lazy val cached: TaskRefData = TaskReferences.lookupOrCreateTaskRef(id, None)

  def task: Signal[DeltaBufferRDT[CausalLastWriterWinsRegister[TaskData]]] = cached.task
  def tag: TypedTag[LI]                                                    = cached.tag
  def removed: Event[String]                                               = cached.removed
}

final class TaskRefData(
    val task: Signal[DeltaBufferRDT[CausalLastWriterWinsRegister[TaskData]]],
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

object TaskReferences {
  private val taskRefMap: mutable.Map[String, TaskRefData] = mutable.Map.empty

  var taskrefObj: TaskReferences = null

  def lookupOrCreateTaskRef(id: String, task: Option[TaskData]): TaskRefData = {
    TaskReferences.taskRefMap.getOrElseUpdate(id, { taskrefObj.createTaskRef(id, task) })
  }

  def apply(toggleAll: Event[UIEvent], storePrefix: String): TaskReferences = {
    val taskrefs = new TaskReferences(toggleAll, storePrefix)
    TaskReferences.taskrefObj = taskrefs
    taskrefs
  }

  val taskBinding    = Binding[DeltaFor[CausalLastWriterWinsRegister[TaskData]] => Unit]("todo task")
  val taskReplicator = ReplicationGroup(rescala.default, Todolist.registry, taskBinding)
}

class TaskReferences(toggleAll: Event[UIEvent], storePrefix: String) {

  def createTaskRef(
      taskID: String,
      task: Option[TaskData],
  ): TaskRefData = {
    val lwwInit = DeltaBufferRDT(replicaId, CausalLastWriterWinsRegister.empty[TaskData])

    val lww: DeltaBufferRDT[CausalLastWriterWinsRegister[TaskData]] = task match {
      case None =>
//        type L = CausalLastWriterWinsRegister[TaskData]
//        given perm1: PermCausalMutate[DeltaBufferRDT[L], L] =
//          DeltaBufferRDT.contextPermissions[CausalLastWriterWinsRegister[TaskData]]
//        given perm2: PermId[DeltaBufferRDT[L]] = DeltaBufferRDT.contextPermissions

        lwwInit.applyDelta {
          MultiVersionRegister.syntax(DottedName(lwwInit.replicaID, lwwInit.state.map(_.repr))).write(TimedVal(
            TaskData("<empty>"),
            0,
            lwwInit.replicaID,
            0
          )).map(CausalLastWriterWinsRegister.apply)
        }
      case Some(v) => lwwInit.write(v)
    }

    val edittext = Events.fromCallback[UIEvent] { inputChange =>
      input(`class` := "edit", `type` := "text", onchange := inputChange, onblur := inputChange)
    }

    val edittextStr = edittext.event.map { (e: UIEvent) =>
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

    val deltaEvt = Evt[DottedName[CausalLastWriterWinsRegister[TaskData]]]()

    // type Carrier = CausalLastWriterWinsRegister.State[TaskData, DietMapCContext]
    //
    // val merge = implicitly[DecomposeLattice[Carrier]]
    //
    // val crdtAlt = DeltaStateReactive.create[Carrier, Carrier](
    //  lww,
    //  deltaEvt,
    //  (s, d) => merge.merge(s, d),
    //  Seq(
    //    { (dt: DynamicTicket, current: Carrier) => dt.depend(doneEv); current },
    //  )
    // )

    val crdt = Storing.storedAs(s"$storePrefix$taskID", lww) { init =>
      Events.foldAll(init)(current =>
        Seq(
          doneEv act2 { _ => current.resetDeltaBuffer().map(_.toggle()) },
          edittextStr act2 { v => current.resetDeltaBuffer().map(_.edit(v)) },
          deltaEvt act2 { delta => current.resetDeltaBuffer().applyDelta(delta) }
        )
      )
    }(Codecs.codecLww)

    TaskReferences.taskReplicator.distributeDeltaRDT(taskID, crdt, deltaEvt)

    val taskData =
      crdt.map(x => x.read.getOrElse(TaskData(desc = "LWW Empty")))

    val removeButton =
      Events.fromCallback[UIEvent](cb => button(`class` := "destroy", onclick := cb))

    val editInput = edittext.data(value := taskData.map(_.desc)).render
    editDiv.event.observe { _ =>
      setTimeout(0) { editInput.focus() }; ()
    }

    val listItem = li(
      `class` := editingV.map(if (_) "editing" else "no-editing"),
      editDiv.data(
        input(
          `class` := "toggle",
          `type`  := "checkbox",
          doneClick.data,
          checked := taskData.map(c => if (c.done) Some(checked.v) else None)
        ),
        label(taskData.map(c => stringFrag(c.desc)).asModifier),
        removeButton.data
      ),
      editInput
    )

    new TaskRefData(crdt, listItem, removeButton.event.map(_ => taskID), taskID)
  }
}
