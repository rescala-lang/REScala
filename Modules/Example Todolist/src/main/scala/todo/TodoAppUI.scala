package todo

import org.scalajs.dom
import org.scalajs.dom.html.{Div, Input, LI}
import org.scalajs.dom.{HTMLDivElement, window}
import rdts.base.Bottom
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import reactives.default.*
import reactives.extra.Tags.*
import replication.Storing
import scalatags.JsDom
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.section
import todo.Codecs.given
import todo.TodoDataManager.TodoRepState
import todo.Todolist.replicaId

class TodoAppUI(val storagePrefix: String) {

  def getContents(): Div = {

    val todoInputTag: Input = input(
      id          := "newtodo",
      `class`     := "new-todo",
      placeholder := "What needs to be done?",
      autofocus   := "autofocus",
      `type`      := "text"
    ).render

    val createTodo = todoInputTag.inputEntered

    val removeAll = Event.fromCallback(button("remove all done todos", onclick := Event.handle))

    val toggleAll = Event.fromCallback {
      input(
        id       := "toggle-all",
        name     := "toggle-all",
        `class`  := "toggle-all",
        `type`   := "checkbox",
        onchange := Event.handle[dom.Event]
      ).render
    }

    val taskrefs = TaskReferences(toggleAll.event, storagePrefix)
    val taskOps  = new TaskOps(taskrefs, replicaId)

    val tasksRDT: Signal[DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]] =
      Storing.storedAs(storagePrefix, DeltaBuffer(Dotted(ReplicatedList.empty[TaskRef]))) { init =>
        Fold(init)(
          taskOps.handleCreateTodo(createTodo),
          taskOps.handleRemoveAll(removeAll.event),
          Fold.branch {
            current.toList.flatMap(_.removed.value).foldLeft(current) { (c, e) => taskOps.handleRemove(c)(e) }
          },
          // todo: does not restore full state
          taskOps.handleDelta(TodoDataManager.dataManager.changes)
        )
      }

    TodoDataManager.publish(tasksRDT, list => Bottom.empty[TodoRepState].copy(list = list))

    val tasksList: Signal[List[TaskRef]] = tasksRDT.map { _.toList }
    val tasksData: Signal[List[TaskData]] =
      Signal.dynamic { tasksList.value.flatMap(l => l.task.value.state.data.read) }
    val taskTags: Signal[List[LI]] = Signal { tasksList.value.map(_.tag) }

    val largeheader = window.location.hash.drop(1)

    val hideEmpty = Signal:
      `style` := (if tasksData.value.isEmpty then "display:none" else "")

    div(
      `class` := "appcontainer",
      div(
        `class` := "todoapp",
        header(
          `class` := "header",
          h1(if largeheader.nonEmpty then largeheader else "todos"),
          todoInputTag
        ),
        section(
          `class` := "main",
          toggleAll.data,
          label(`for` := "toggle-all", "Mark all as complete"),
          ul(
            `class` := "todo-list",
          ).render.reattach(taskTags)
        ).render.reattach(hideEmpty),
        div(
          `class` := "footer",
          span(
            `class` := "todo-count",
          ).render.reattach(
            Signal {
              val remainingTasks = tasksData.value.count(!_.done)
              println(s"remaining observer")
              List(
                strong(remainingTasks.toString).render,
                span(if remainingTasks == 1 then
                  " item left"
                else " items left").render
              )
            }
          )
        ).render.reattach(hideEmpty)
          .reattach {
            Signal {
              removeAll.data(
                `class` := s"clear-completed${if !tasksData.value.exists(_.done) then " hidden" else ""}"
              ).render
            }
          }
      )
    ).render
  }

}
