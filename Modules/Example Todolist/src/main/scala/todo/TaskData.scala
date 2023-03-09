package todo

import kofre.datatypes.{LastWriterWins, MultiVersionRegister, alternatives}
import kofre.dotted.Dotted
import kofre.syntax.{DeltaBuffer, PermCausalMutate, ReplicaId}
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
import kofre.base.Bottom
import kofre.datatypes.alternatives.MultiValueRegister
import kofre.datatypes.alternatives.lww.{TimedVal, WallClock}
import kofre.time.Dots
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

  def task: Signal[DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]]] = cached.task
  def tag: TypedTag[LI]                                                   = cached.tag
  def removed: Event[String]                                              = cached.removed
}

final class TaskRefData(
    val task: Signal[DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]]],
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

  val taskBinding = Binding[DeltaFor[LastWriterWins[Option[TaskData]]] => Unit]("todo task")
  val taskReplicator =
    given Bottom[LastWriterWins[Option[TaskData]]] with {
      override def empty: LastWriterWins[Option[TaskData]] = null
    }
    ReplicationGroup(rescala.default, Todolist.registry, taskBinding)
}

class TaskReferences(toggleAll: Event[UIEvent], storePrefix: String) {
  given fixedId: ReplicaId = replicaId

  def createTaskRef(
      taskID: String,
      task: Option[TaskData],
  ): TaskRefData = {
    val lww: DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]] =
      val dot = Dots.empty.nextDot(fixedId.uid)
      DeltaBuffer(Dotted(LastWriterWins.now(dot, task)))

    val edittext = Event.fromCallback {
      input(`class` := "edit", `type` := "text", onchange := Event.handle[UIEvent], onblur := Event.handle[UIEvent])
    }

    val edittextStr = edittext.event.map { (e: UIEvent) =>
      val myinput = e.target.asInstanceOf[Input]
      myinput.value.trim
    }

    val editDiv = Event.fromCallback {
      div(`class` := "view", ondblclick := Event.handle)
    }

    val changeEditing = (edittextStr map const(false)) || (editDiv.event map const(true))
    val editingV      = changeEditing.hold(init = false)

    val doneClick = Event.fromCallback(onchange := Event.handle)

    val doneEv = toggleAll || doneClick.event

    val deltaEvt = Evt[Dotted[LastWriterWins[Option[TaskData]]]]()

    val crdt = Storing.storedAs(s"$storePrefix$taskID", lww) { init =>
      Fold(init)(
        doneEv act { _ => current.clearDeltas().map(_.toggle()) },
        edittextStr act { v => current.clearDeltas().map(_.edit(v)) },
        deltaEvt act { delta => current.clearDeltas().applyDelta(delta) }
      )
    }(Codecs.codecLww)

    TaskReferences.taskReplicator.distributeDeltaRDT(taskID, crdt, deltaEvt)

    val taskData =
      crdt.map(x => x.read.getOrElse(TaskData(desc = "LWW Empty")))

    val removeButton =
      Event.fromCallback(button(`class` := "destroy", onclick := Event.handle))

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
