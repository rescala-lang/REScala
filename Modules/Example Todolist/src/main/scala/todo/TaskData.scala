package todo

import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.html.{Input, LI}
import rdts.base.Bottom
import rdts.datatypes.LastWriterWins
import rdts.dotted.{Dotted, DottedLattice}
import rdts.syntax.{DeltaBuffer, LocalReplicaId}
import reactives.default.*
import reactives.extra.Tags.*
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all.*
import todo.Codecs.given
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

given [A]: DottedLattice[LastWriterWins[A]] = Dotted.liftLattice

case class TaskRef(id: String) {
  lazy val cached: TaskRefData = TaskReferences.lookupOrCreateTaskRef(id, None)

  def task: Signal[DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]]] = cached.task
  def tag: LI                                                             = cached.tag
  def removed: Event[String]                                              = cached.removed
}

final class TaskRefData(
    val task: Signal[DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]]],
    val tag: dom.html.LI,
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

  def apply(toggleAll: Event[dom.Event], storePrefix: String): TaskReferences = {
    val taskrefs = new TaskReferences(toggleAll, storePrefix)
    TaskReferences.taskrefObj = taskrefs
    taskrefs
  }
}

class TaskReferences(toggleAll: Event[dom.Event], storePrefix: String) {
  given fixedId: LocalReplicaId = replicaId

  def createTaskRef(
      taskID: String,
      task: Option[TaskData],
  ): TaskRefData = {
    val lww: DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]] =
      if task.isEmpty
      then DeltaBuffer(Dotted(LastWriterWins.fallback(task)))
      else DeltaBuffer(Dotted(LastWriterWins.now(task)))

    val edittext: Event.CBR[dom.Event, dom.html.Input] = Event.fromCallback {
      input(
        `class`  := "edit",
        `type`   := "text",
        onchange := Event.handle[dom.Event],
        onblur   := Event.handle[dom.Event]
      ).render
    }

    val edittextStr = edittext.event.map { (e: dom.Event) =>
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

    val remoteUpdates = GlobalRegistry.subscribeBranch[LastWriterWins[Option[TaskData]]](taskID)

    extension (db: DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]])
      def modTask(f: TaskData => TaskData): DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]] =
        db.transform(_.map(_.map(f)))

    val crdt: Signal[DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]]] =
      Storing.storedAs(s"$storePrefix$taskID", lww) { init =>
        Fold(init)(
          doneEv act { _ => current.clearDeltas().modTask(_.toggle()) },
          edittextStr act { v => current.clearDeltas().modTask(_.edit(v)) },
          remoteUpdates
        )
      }(using Codecs.codecLww)

    GlobalRegistry.publish(taskID, crdt)
    GlobalRegistry.unbuffer(taskID)

    val taskData = Signal {
      crdt.value.state.data.read.getOrElse(TaskData(desc = "LWW Empty"))
    }

    val removeButton =
      Event.fromCallback(button(`class` := "destroy", onclick := Event.handle))

    val editInput = edittext.data.reattach(Signal { value := taskData.value.desc })
    editDiv.event.observe { _ =>
      setTimeout(0) { editInput.focus() }; ()
    }

    val listItem = li(
      editDiv.data(
        input(
          `class` := "toggle",
          `type`  := "checkbox",
          doneClick.data,
        ).render.reattach(Signal:
          if taskData.value.done
          then
            (elem: dom.html.Input) =>
              elem.checked = true
              elem.setAttribute("checked", "checked")
          else
            (elem: dom.html.Input) =>
              elem.checked = false
              elem.removeAttribute("checked")
        ),
        label.render.reattach(taskData.map(c => c.desc)),
        removeButton.data
      ),
      editInput
    ).render.reattach(Signal {
      if editingV.value
      then (elem: dom.Element) => elem.setAttribute("class", "editing")
      else (elem: dom.Element) => elem.setAttribute("class", "no-editing")
    })

    new TaskRefData(crdt, listItem, removeButton.event.map(_ => taskID), taskID)
  }
}

given RangeSplice[dom.Element, Modifier] with {
  override def splice(anchor: dom.Element, range: dom.Range, value: Modifier): Unit =
    println(s"applying $value to $anchor")
    anchor match
      case elem: dom.Element => value.applyTo(elem)
}

given [A <: dom.Element]: RangeSplice[A, A => Unit] with {
  override def splice(anchor: A, range: dom.Range, value: A => Unit): Unit =
    anchor match
      case elem: A => value.apply(elem)
}

implicit def optionAttrValue[T](implicit ev: AttrValue[T]): AttrValue[Option[T]] =
  new AttrValue[Option[T]] {
    override def apply(t: Element, a: Attr, v: Option[T]): Unit = {
      v match {
        case Some(value) => ev.apply(t, a, value)
        case None =>
          a.namespace match {
            case None     => t.removeAttribute(a.name)
            case Some(ns) => t.removeAttributeNS(ns.uri, a.name)
          }
      }
    }
  }
