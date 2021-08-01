package src.main.todo

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import loci.registry.Binding
import loci.serializer.jsoniterScala.jsoniteScalaBasedSerializable
import loci.transmitter.transmittable.IdenticallyTransmittable
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Div, Input, LI}
import rescala.default._
import rescala.extra.Tags._
import rescala.extra.distributables.LociDist
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.Codecs._
import rescala.extra.lattices.delta.Delta
import rescala.extra.lattices.delta.crdt.reactive.RGA._
import rescala.extra.lattices.delta.crdt.reactive.{LWWRegister, RGA}
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import scalatags.JsDom.{Attr, TypedTag}

class TodoAppUI() {

  implicit val stringCodec: JsonValueCodec[String] = JsonCodecMaker.make

  implicit val transmittableList: IdenticallyTransmittable[RGA.State[String, DietMapCContext]] =
    IdenticallyTransmittable()

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

    val deltaEvt = Evt[Delta[RGA.State[String, DietMapCContext]]]

    val signalMapInitial = Map[String, Signal[LWWRegister[TodoTask, DietMapCContext]]]()

    val uiMapInitial = Map[String, TypedTag[LI]]()

    val evtMapInitial = Map[String, Event[String]]()

    val tasklist = new TaskList(toggleAll.event)

    val state =
      Events.foldAll(tasklist.State(tasklist.listInitial, signalMapInitial, uiMapInitial, evtMapInitial)) { s =>
        Seq(
          createTodo act tasklist.handleCreateTodo(s),
          removeAll.event dyn { dt => _ => tasklist.handleRemoveAll(s, dt) },
          s.evtMap.values.toSeq act tasklist.handleRemove(s),
          deltaEvt act tasklist.handleDelta(s)
        )
      }

    val rga = state.map(_.list)

    LociDist.distributeDeltaCRDT(rga, deltaEvt, Todolist.registry)(
      Binding[RGA.State[String, DietMapCContext] => Unit]("tasklist")
    )

    val tasksAndListItems = state.map { s =>
      val list      = s.list.toList
      val tasks     = list.flatMap(id => s.signalMap(id)().read)
      val listItems = list.map(id => s.uiMap(id))
      (tasks, listItems)
    }

    val tasks = tasksAndListItems.map(_._1)

    val listItems = tasksAndListItems.map(_._2)

    div(
      `class` := "todoapp",
      header(
        `class` := "header",
        h1("todos"),
        todoInputField
      ),
      section(
        `class` := "main",
        `style` := Signal { if (tasks().isEmpty) "display:hidden" else "" },
        toggleAll.value,
        label(`for` := "toggle-all", "Mark all as complete"),
        ul(
          `class` := "todo-list",
          listItems.asModifierL
        )
      ),
      div(
        `class` := "footer",
        `style` := Signal { if (tasks().isEmpty) "display:none" else "" },
        Signal {
          val remainingTasks = tasks.value.count(!_.done)
          span(
            `class` := "todo-count",
            strong("" + remainingTasks),
            span(if (remainingTasks == 1)
              " item left"
            else " items left")
          )
        }.asModifier,
        Signal {
          removeAll.value(`class` := "clear-completed" + (if (!tasks.value.exists(_.done)) " hidden" else ""))
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
