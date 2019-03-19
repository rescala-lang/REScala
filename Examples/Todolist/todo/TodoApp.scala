package todo

import io.circe.generic.auto._
import loci.registry.Binding
import loci.serializer.circe._
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Div, Input}
import rescala.Tags._
import rescala.distributables.LociDist
import rescala.lattices.sequences.RGA
import rescala.lattices.sequences.RGA.RGA
import rescala.restoration.LocalStorageStore
import rescala.restoration.ReCirce._
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import scalatags.JsDom.{Attr, TypedTag}

object TodoApp{
  def apply(taskHandling: TaskHandling)(implicit storingScheduler: LocalStorageStore)
  : TodoApp[taskHandling.type] = new TodoApp(taskHandling)
}

class TodoApp[TH <: TaskHandling](val taskHandling: TH)(implicit val storingScheduler: LocalStorageStore) {

  import storingScheduler._
  import taskHandling.{maketask, toggleAll}

  case class TodoRes(div: TypedTag[Div], tasklist: Signal[RGA[taskHandling.Taskref]])

  def getContents(): TodoRes = {

    val todoInputTag: JsDom.TypedTag[Input] = input(
      id := "newtodo",
      `class` := "new-todo",
      placeholder := "What needs to be done?",
      autofocus := "autofocus")

    val (createTodo, todoInputField) = inputFieldHandler(todoInputTag, onchange)


    val innerTasks = List(
      maketask(TaskData("walk the dog"), "initdog"),
      maketask(TaskData("get milk"), "initmilk"),
      maketask(TaskData("get coffee"), "initcoffe")
    )


    val removeAll =
      Events.fromCallback[UIEvent](cb => button("remove all done todos", onclick := cb))


    val createTask = createTodo.map { str => maketask(TaskData(str)) }

    val tasksRGA = Events.foldAll(RGA(innerTasks)) { tasks =>
      Seq(
        createTask >> {tasks.prepend},
        removeAll.event >>> { dt => _ => tasks.filter(t => !dt.depend(t.contents).done) },
        tasks.value.map(_.removeClick) >> { t => tasks.filter(_.id != t) }
        )
    }(implicitly, "tasklist")

    LociDist.distribute(tasksRGA, Todolist.registry)(Binding("tasklist"))

    val tasks = tasksRGA.map(_.value.reverse)


    val content = div(
      `class` := "todoapp",
      header(
        `class` := "header",
        h1("todos"),
        todoInputField
      ),

      section(
        `class` := "main",
        `style` := Signal {if (tasks().isEmpty) "display:hidden" else ""},
        toggleAll.value,
        label(`for` := "toggle-all", "Mark all as complete"),
        tasks.map(l =>
                    ul(
                      `class` := "todo-list",
                      l.map(_.listItem))).asModifier
      ),
      div(
        `class` := "footer",
        `style` := Signal {if (tasks().isEmpty) "display:none" else ""},

        Signal.dynamic {
          val remainingTasks = tasks.value.filter(!_.contents.value.done)
          span(
            `class` := "todo-count",
            strong("" + remainingTasks.size),
            span(if (remainingTasks.size == 1)
                   " item left" else " items left")
          )
        }.asModifier,

        removeAll.value(`class` := Signal.dynamic {
          "clear-completed" +
          (if (!tasks().exists(t => t.contents.value.done)) " hidden" else "")
        })
      )
    )

    TodoRes(content, tasksRGA)
  }

  def inputFieldHandler(tag: TypedTag[Input], attr: Attr): (Event[String], Input) = {
    val handler = Events.fromCallback[UIEvent](cb => tag(attr := cb))

    val todoInputField: Input = handler.value.render

    (handler.event.map { e: UIEvent =>
      e.preventDefault()
      val res = todoInputField.value.trim
      todoInputField.value = ""
      res
    }, todoInputField)
  }

}
