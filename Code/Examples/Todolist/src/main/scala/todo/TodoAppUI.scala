package todo

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.datatypes.ReplicatedList
import loci.registry.Binding
import loci.serializer.jsoniterScala._
import org.scalajs.dom.html.{Div, Input, LI}
import org.scalajs.dom.{UIEvent, window}
import rescala.default.Events.CBResult
import rescala.default._
import rescala.extra.Tags._
import rescala.extra.replication.{DeltaFor, ReplicationGroup}
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import scalatags.JsDom.{Attr, TypedTag}
import todo.Codecs.given
import todo.Todolist.replicaId
import kofre.decompose.interfaces.LWWRegister.LWWRegisterSyntax
import kofre.deprecated.containers.DeltaBufferRDT
import kofre.dotted.Dotted
import kofre.syntax.DottedName
import loci.serializer.jsoniterScala.given

class TodoAppUI(val storagePrefix: String) {

  val tasklistBinding = Binding[DeltaFor[ReplicatedList[TaskRef]] => Unit]("tasklist")
  val tasklistReplicator = new ReplicationGroup(rescala.default, Todolist.registry, tasklistBinding)

  def getContents(): TypedTag[Div] = {

    val todoInputTag: JsDom.TypedTag[Input] = input(
      id          := "newtodo",
      `class`     := "new-todo",
      placeholder := "What needs to be done?",
      autofocus   := "autofocus"
    )

    val createTodo = inputFieldHandler(todoInputTag, onchange)

    val removeAll = Events.fromCallback[UIEvent](cb => button("remove all done todos", onclick := cb))

    val toggleAll = Events.fromCallback[UIEvent] { cb =>
      input(id := "toggle-all", name := "toggle-all", `class` := "toggle-all", `type` := "checkbox", onchange := cb)
    }

    val taskrefs = TaskReferences(toggleAll.event, storagePrefix)
    val taskOps  = new TaskOps(taskrefs)

    val deltaEvt = Evt[DottedName[ReplicatedList[TaskRef]]]()

    val tasksRDT: Signal[DeltaBufferRDT[ReplicatedList[TaskRef]]] =
      Storing.storedAs(storagePrefix, DeltaBufferRDT(replicaId, ReplicatedList.empty[TaskRef])) { init =>
        Fold(init)(
          taskOps.handleCreateTodo(createTodo.event),
          taskOps.handleRemoveAll(removeAll.event),
          Fold.branch {
            current.toList.flatMap(_.removed.value).foldLeft(current){(c, e) => taskOps.handleRemove(c)(e)}
          },
          taskOps.handleDelta(deltaEvt)
        )
      }(Codecs.codecRGA)

    tasklistReplicator.distributeDeltaRDT("tasklist", tasksRDT, deltaEvt)

    val tasksList: Signal[List[TaskRef]] = tasksRDT.map { _.toList }
    val tasksData: Signal[List[TaskData]] =
      Signal.dynamic { tasksList.value.flatMap(l => new LWWRegisterSyntax(l.task.value).read) }
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

  def inputFieldHandler(tag: TypedTag[Input], attr: Attr): CBResult[String, Input] = {
    val handler = Events.fromCallback[UIEvent](cb => tag(attr := cb))

    val todoInputField: Input = handler.data.render

    val handlerEvent =
      handler.event.map { (e: UIEvent) =>
        e.preventDefault()
        val res = todoInputField.value.trim
        todoInputField.value = ""
        res
      }

    new CBResult(handlerEvent, todoInputField)
  }

}
