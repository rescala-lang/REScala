package todo

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.datatypes.contextual.ReplicatedList
import loci.registry.Binding
import org.scalajs.dom.html.{Div, Input, LI}
import org.scalajs.dom.{UIEvent, window}
import rescala.default.*
import rescala.extra.Tags.*
import rescala.extra.replication.{DeltaFor, ReplicationGroup}
import scalatags.JsDom
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.section
import scalatags.JsDom.{Attr, TypedTag}
import todo.Codecs.given
import todo.Todolist.replicaId
import kofre.dotted.Dotted
import kofre.syntax.DeltaBuffer
import loci.serializer.jsoniterScala.given

class TodoAppUI(val storagePrefix: String) {

  val tasklistBinding    = Binding[DeltaFor[ReplicatedList[TaskRef]] => Unit]("tasklist")
  val tasklistReplicator = new ReplicationGroup(rescala.default, Todolist.registry, tasklistBinding)

  def getContents(): TypedTag[Div] = {

    val todoInputTag: JsDom.TypedTag[Input] = input(
      id          := "newtodo",
      `class`     := "new-todo",
      placeholder := "What needs to be done?",
      autofocus   := "autofocus"
    )

    val createTodo = inputFieldHandler(todoInputTag, onchange)

    val removeAll = Event.fromCallback(button("remove all done todos", onclick := Event.handle))

    val toggleAll = Event.fromCallback {
      input(
        id       := "toggle-all",
        name     := "toggle-all",
        `class`  := "toggle-all",
        `type`   := "checkbox",
        onchange := Event.handle[UIEvent]
      )
    }

    val taskrefs = TaskReferences(toggleAll.event, storagePrefix)
    val taskOps  = new TaskOps(taskrefs, replicaId)

    val deltaEvt = Evt[Dotted[ReplicatedList[TaskRef]]]()

    val tasksRDT: Signal[DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]] =
      Storing.storedAs(storagePrefix, DeltaBuffer(Dotted(ReplicatedList.empty[TaskRef]))) { init =>
        Fold(init)(
          taskOps.handleCreateTodo(createTodo.event),
          taskOps.handleRemoveAll(removeAll.event),
          Fold.branch {
            current.toList.flatMap(_.removed.value).foldLeft(current) { (c, e) => taskOps.handleRemove(c)(e) }
          },
          taskOps.handleDelta(deltaEvt)
        )
      }(Codecs.codecRGA)

    tasklistReplicator.distributeDeltaRDT("tasklist", tasksRDT, deltaEvt)

    val tasksList: Signal[List[TaskRef]] = tasksRDT.map { _.toList }
    val tasksData: Signal[List[TaskData]] =
      Signal.dynamic { tasksList.value.flatMap(l => l.task.value.read) }
    val taskTags: Signal[List[TypedTag[LI]]] = Signal { tasksList.value.map(_.tag) }

    val largeheader = window.location.hash.drop(1)

    div(
      `class` := "todoapp",
      header(
        `class` := "header",
        h1(if (largeheader.nonEmpty) largeheader else "todos"),
        createTodo.data
      ),
      section(
        `class` := "main",
        `style` := Signal { if (tasksData.value.isEmpty) "display:hidden" else "" },
        toggleAll.data,
        label(`for` := "toggle-all", "Mark all as complete"),
        ul(
          `class` := "todo-list",
          taskTags.asModifierL
        )
      ),
      div(
        `class` := "footer",
        `style` := Signal { if (tasksData.value.isEmpty) "display:none" else "" },
        Signal {
          val remainingTasks = tasksData.value.count(!_.done)
          span(
            `class` := "todo-count",
            strong("" + remainingTasks),
            span(if (remainingTasks == 1)
              " item left"
            else " items left")
          )
        }.asModifier,
        Signal {
          removeAll.data(`class` := s"clear-completed${if (!tasksData.value.exists(_.done)) " hidden" else ""}")
        }.asModifier
      )
    )
  }

  def inputFieldHandler(tag: TypedTag[Input], attr: Attr): Event.CBR[String, Input] = {
    val handler = Event.fromCallback(tag(attr := Event.handle[UIEvent]))

    val todoInputField: Input = handler.data.render

    val handlerEvent =
      handler.event.map { (e: UIEvent) =>
        e.preventDefault()
        val res = todoInputField.value.trim
        todoInputField.value = ""
        res
      }

    Event.CBR(handlerEvent, todoInputField)
  }

}
