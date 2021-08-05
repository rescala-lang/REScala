package todo

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import loci.registry.Binding
import loci.serializer.jsoniterScala._
import loci.transmitter.transmittable.IdenticallyTransmittable
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Div, Input}
import rescala.default._
import rescala.extra.Tags._
import rescala.extra.distributables.LociDist
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.Codecs._
import rescala.extra.lattices.delta.Delta
import rescala.extra.lattices.delta.crdt.reactive.RGA
import rescala.extra.lattices.delta.crdt.reactive.RGA._
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import scalatags.JsDom.{Attr, TypedTag}

class TodoAppUI() {

  implicit val stringCodec: JsonValueCodec[String] = JsonCodecMaker.make

  @scala.annotation.nowarn // Auto-application to `()`
  def getContents(): TypedTag[Div] = {

    val todoInputTag: JsDom.TypedTag[Input] = input(
      id          := "newtodo",
      `class`     := "new-todo",
      placeholder := "What needs to be done?",
      autofocus   := "autofocus"
    )

    val (createTodo, todoInputField) = inputFieldHandler(todoInputTag, onchange)

    val removeAll = Events.fromCallback[UIEvent](cb => button("remove all done todos", onclick := cb))

    val toggleAll = Events.fromCallback[UIEvent] { cb =>
      input(id := "toggle-all", name := "toggle-all", `class` := "toggle-all", `type` := "checkbox", onchange := cb)
    }

    val deltaEvt = Evt[Delta[RGA.State[TaskRef, DietMapCContext]]]

    val taskrefs = new TaskRefObj(toggleAll.event)
    val tasklist = new TaskList(toggleAll.event, taskrefs)

    import taskrefs.taskRefCodec
    implicit val codec: JsonValueCodec[RGA.State[TaskRef, DietMapCContext]] = RGAStateCodec

    implicit val transmittableList: IdenticallyTransmittable[RGA.State[TaskRef, DietMapCContext]] =
      IdenticallyTransmittable()

    val rga =
      Events.foldAll(tasklist.listInitial) { s =>
        Seq(
          createTodo act tasklist.handleCreateTodo(s),
          removeAll.event dyn { dt => _ => tasklist.handleRemoveAll(s, dt) },
          s.toList.map(_.removed) act tasklist.handleRemove(s),
          deltaEvt act tasklist.handleDelta(s)
        )
      }

    LociDist.distributeDeltaCRDT(rga, deltaEvt, Todolist.registry)(
      Binding[RGA.State[TaskRef, DietMapCContext] => Unit]("tasklist")
    )

    val tasksList = rga.map { _.toList }

    val tasksData = tasksList.map(_.flatMap(_.task.value.read))

    val taskTags = tasksList.map(_.map(_.tag))

    div(
      `class` := "todoapp",
      header(
        `class` := "header",
        h1("todos"),
        todoInputField
      ),
      section(
        `class` := "main",
        `style` := Signal { if (tasksData().isEmpty) "display:hidden" else "" },
        toggleAll.value,
        label(`for` := "toggle-all", "Mark all as complete"),
        ul(
          `class` := "todo-list",
          taskTags.asModifierL
        )
      ),
      div(
        `class` := "footer",
        `style` := Signal { if (tasksData().isEmpty) "display:none" else "" },
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
          removeAll.value(`class` := "clear-completed" + (if (!tasksData.value.exists(_.done)) " hidden" else ""))
        }.asModifier
      )
    )
  }

  def inputFieldHandler(tag: TypedTag[Input], attr: Attr): (Event[String], Input) = {
    val handler = Events.fromCallback[UIEvent](cb => tag(attr := cb))

    val todoInputField: Input = handler.value.render

    (
      handler.event.map { e: UIEvent =>
        e.preventDefault()
        val res = todoInputField.value.trim
        todoInputField.value = ""
        res
      },
      todoInputField
    )
  }

}
