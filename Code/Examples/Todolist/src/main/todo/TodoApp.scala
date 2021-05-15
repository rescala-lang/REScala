package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import loci.MessageBuffer
import loci.registry.Binding
import loci.transmitter.transmittable.IdenticallyTransmittable
import loci.transmitter.Serializable
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Div, Input}
import rescala.default._
import rescala.extra.Tags._
import rescala.extra.distributables.LociDist
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.Delta
import rescala.extra.lattices.delta.crdt.{LastWriterWins, LastWriterWinsCRDT, RORMap, RRGA}
import rescala.extra.lattices.delta.crdt.RRGA._
import rescala.extra.lattices.delta.crdt.RGACRDT._
import rescala.extra.lattices.delta.crdt.RORMap._
import rescala.extra.lattices.delta.crdt.LastWriterWins._
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import scalatags.JsDom.{Attr, TypedTag}

import java.util.concurrent.ThreadLocalRandom
import scala.util.Try

class TodoApp() {

  implicit def jsoniterBasedSerializable[T](implicit codec: JsonValueCodec[T]): Serializable[T] = new Serializable[T] {
    override def serialize(value: T): MessageBuffer = MessageBuffer.encodeString(writeToString(value))

    override def deserialize(value: MessageBuffer): Try[T] = Try(readFromString[T](value.decodeString))
  }

  implicit val todoTaskCodec: JsonValueCodec[TodoTask] = JsonCodecMaker.make
  implicit val stringCodec: JsonValueCodec[String]     = JsonCodecMaker.make

//  implicit val transmittableTaskList: IdenticallyTransmittable[RRGA.State[TodoTask, DietMapCContext]] =
//    IdenticallyTransmittable()

  implicit val transmittableList: IdenticallyTransmittable[RRGA.State[String, DietMapCContext]] =
    IdenticallyTransmittable()

  implicit val transmittableTaskMap
      : IdenticallyTransmittable[RORMap.State[String, LastWriterWins.Embedded[TodoTask], DietMapCContext]] =
    IdenticallyTransmittable()

  case class ViewDataPair(view: Map[String, TodoTaskView], data: RRGA[TodoTask, DietMapCContext])

  @scala.annotation.nowarn // Auto-application to `()`
  def getContents(): TypedTag[Div] = {

    val todoInputTag: JsDom.TypedTag[Input] = input(
      id := "newtodo",
      `class` := "new-todo",
      placeholder := "What needs to be done?",
      autofocus := "autofocus"
    )

    val (createTodo, todoInputField) = inputFieldHandler(todoInputTag, onchange)

    val newTask = createTodo.map { desc =>
      TodoTask(s"Task(${ThreadLocalRandom.current().nextLong().toHexString})", desc)
    }

    val removeAll = Events.fromCallback[UIEvent](cb => button("remove all done todos", onclick := cb))

    val toggleAll = Events.fromCallback[UIEvent] { cb =>
      input(id := "toggle-all", name := "toggle-all", `class` := "toggle-all", `type` := "checkbox", onchange := cb)
    }

    val myID = ThreadLocalRandom.current().nextLong().toHexString

//    val deltaEvt: Evt[Delta[RRGA.State[TodoTask, DietMapCContext]]] = Evt()

    val listDeltaEvt: Evt[Delta[RRGA.State[String, DietMapCContext]]] = Evt()

    val mapDeltaEvt: Evt[Delta[RORMap.State[String, LastWriterWins.Embedded[TodoTask], DietMapCContext]]] = Evt()

//    val initial = ViewDataPair(Map.empty[String, TodoTaskView], RRGA[TodoTask, DietMapCContext](myID))

    val listInitial = RRGA[String, DietMapCContext](myID)

    case class VDP(
        view: Map[String, TodoTaskView],
        data: RORMap[String, LastWriterWins.Embedded[TodoTask], DietMapCContext]
    )

    val pairInitial = VDP(
      Map.empty[String, TodoTaskView],
      RORMap[String, LastWriterWins.Embedded[TodoTask], DietMapCContext](myID)
    )

    val pair = Events.foldAll(pairInitial) { p =>
      Seq(
        newTask act { task =>
          val newMap = p.data.mutateKey(task.id, LastWriterWinsCRDT.write(task))
          val newUI  = p.view + (task.id -> TodoTaskView.fromTask(task))

          VDP(newUI, newMap)
        },
        removeAll.event act { _ =>
          val doneIDs = p.data.queryAllEntries(LastWriterWinsCRDT.read).collect {
            case Some(TodoTask(id, _, true)) => id
          }

          VDP(p.view.removedAll(doneIDs), p.data.removeAll(doneIDs))
        },
        OnEvs(p.view.values.toList.map(_.removeEvt)) act { id =>
          val newMap = p.data.removeByValue { v =>
            LastWriterWinsCRDT.read[TodoTask, DietMapCContext].apply(v) match {
              case None                    => false
              case Some(TodoTask(i, _, _)) => i == id
            }
          }

          VDP(p.view.removed(id), newMap)
        },
        OnEvs(p.view.values.toList.map(_.writeEvt)) act { task =>
          val newMap = p.data.mutateKey(task.id, LastWriterWinsCRDT.write(task))

          val newUI = p.view.updated(task.id, TodoTaskView.fromTask(task))

          VDP(newUI, newMap)
        },
        mapDeltaEvt act { delta =>
          val newMap = p.data.applyDelta(delta)

          val oldTasks = p.data.queryAllEntries(LastWriterWinsCRDT.read).flatten.toSet
          val newTasks = newMap.queryAllEntries(LastWriterWinsCRDT.read).flatten

          val newUI = newTasks.map { t =>
            if (oldTasks.contains(t)) t.id -> p.view(t.id)
            else t.id                      -> TodoTaskView.fromTask(t)
          }.toMap

          VDP(newUI, newMap)
        }
      )
    }

    val view    = pair.map(_.view)
    val taskMap = pair.map(_.data)

    LociDist.distributeDeltaCRDT(taskMap, mapDeltaEvt, Todolist.registry)(
      Binding[RORMap.State[String, LastWriterWins.Embedded[TodoTask], DietMapCContext] => Unit]("taskMap")
    )

    val mapChanged = pair.map(_.data).changed

    val list = Events.foldAll(listInitial) { state =>
      Seq(
        newTask act {
          case TodoTask(id, _, _) => state.prepend(id)
        },
        mapChanged act { m =>
          state.deleteBy { s =>
            m.queryKey(s, LastWriterWinsCRDT.read[TodoTask, DietMapCContext]) match {
              case None    => true
              case Some(_) => false
            }
          }
        },
        listDeltaEvt act state.applyDelta
      )
    }

    LociDist.distributeDeltaCRDT(list, listDeltaEvt, Todolist.registry)(
      Binding[RRGA.State[String, DietMapCContext] => Unit]("list")
    )

    val tasks = list.map {
      _.toList.flatMap { id =>
        taskMap().queryKey(id, LastWriterWinsCRDT.read)
      }
    }

    val listItems = list.map {
      _.toList.flatMap { id =>
        view().get(id).map(_.tag)
      }
    }

//    val dataPlusUI = Events.foldAll(initial) { p =>
//      Seq(
//        createTodo act { str =>
//          val task    = TodoTask(desc = str)
//          val newList = p.data.prepend(task)
//          val ui      = TodoTaskView.fromTask(task)
//
//          ViewDataPair(p.view + (task.id -> ui), newList)
//        },
//        removeAll.event act { _ =>
//          val doneIDs = p.data.toList.collect {
//            case TodoTask(id, _, true) => id
//          }
//
//          ViewDataPair(p.view.removedAll(doneIDs), p.data.deleteBy(_.done))
//        },
//        OnEvs(p.view.values.toList.map(_.removeEvt)) act { id =>
//          ViewDataPair(p.view.removed(id), p.data.deleteBy(_.id == id))
//        },
//        OnEvs(p.view.values.toList.map(_.writeEvt)) act { task =>
//          val newList = p.data.updateBy(_.id == task.id, task)
//
//          val ui = TodoTaskView.fromTask(task)
//
//          ViewDataPair(p.view.updated(task.id, ui), newList)
//        },
//        deltaEvt act { delta =>
//          val newList = p.data.applyDelta(delta)
//
//          val oldTasks = p.data.toList
//          val newTasks = newList.toList
//
//          val uiMap = newTasks.map { t =>
//            if (oldTasks.contains(t)) t.id -> p.view(t.id)
//            else t.id                      -> TodoTaskView.fromTask(t)
//          }.toMap
//
//          ViewDataPair(uiMap, newList)
//        }
//      )
//    }
//
//    val taskList = dataPlusUI.map(_.data)
//
//    LociDist.distributeDeltaCRDT(taskList, deltaEvt, Todolist.registry)(
//      Binding[RRGA.State[TodoTask, DietMapCContext] => Unit]("tasklist")
//    )
//
//    val tasks = taskList.map(_.toList)
//
//    val listItems = dataPlusUI.map { p =>
//      p.data.toList.map(t => p.view(t.id).tag)
//    }

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
