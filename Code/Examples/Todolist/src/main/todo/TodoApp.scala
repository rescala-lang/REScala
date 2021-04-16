package todo

import cats.collections.Diet
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto
import loci.registry.Binding
import loci.serializer.circe.circeBasedSerializable
import loci.transmitter.IdenticallyTransmittable
import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Div, Input}
import rescala.default._
import rescala.extra.Tags._
import rescala.extra.distributables.LociDist
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.crdt.RMVRegister.AtomicUIJDLattice
import rescala.extra.lattices.delta.crdt.{LastWriterWins, LastWriterWinsCRDT, RORMap, TimedVal}
import rescala.extra.lattices.delta.{Delta, Dot, UIJDLattice}
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import scalatags.JsDom.{Attr, TypedTag}

import java.util.concurrent.ThreadLocalRandom

class TodoApp() {

  implicit val transmittableTaskMap: IdenticallyTransmittable[RORMap.State[String, LastWriterWins.Embedded[TodoTask], DietMapCContext]] = IdenticallyTransmittable()

  implicit val DotDecoder: Decoder[Dot] = semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val DotEncoder: Encoder[Dot] = semiauto.deriveEncoder: @scala.annotation.nowarn

  implicit val RangeDecoder: Decoder[cats.collections.Range[Int]] = semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val RangeEncoder: Encoder[cats.collections.Range[Int]] = semiauto.deriveEncoder: @scala.annotation.nowarn

  implicit val DietDecoder: Decoder[Diet[Int]] = Decoder.decodeList[cats.collections.Range[Int]].map {
    _.foldLeft(Diet.empty[Int]) {
      (d, r) => d.addRange(r)
    }
  }
  implicit val DietEncoder: Encoder[Diet[Int]] =
    Encoder.encodeList[cats.collections.Range[Int]].contramap(_.foldLeftRange(List.empty[cats.collections.Range[Int]]) {
      (l, r) => r :: l
  })

  implicit val DotFunDecoder: Decoder[DotFun[TodoTask]] = Decoder.decodeList[(Dot, TodoTask)].map(_.toMap)
  implicit val DotFunEncoder: Encoder[DotFun[TodoTask]] = Encoder.encodeList[(Dot, TodoTask)].contramap(_.toList)

  implicit val TimedValDecoder: Decoder[TimedVal[TodoTask]] = semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val TimedValEncoder: Encoder[TimedVal[TodoTask]] = semiauto.deriveEncoder: @scala.annotation.nowarn

  implicit val AnotherDotFunDecoder: Decoder[DotFun[TimedVal[TodoTask]]] = Decoder.decodeList[(Dot, TimedVal[TodoTask])].map(_.toMap)
  implicit val AnotherDotFunEncoder: Encoder[DotFun[TimedVal[TodoTask]]] = Encoder.encodeList[(Dot, TimedVal[TodoTask])].contramap(_.toList)

  implicit val RORMapDecoder: Decoder[RORMap.State[String, LastWriterWins.Embedded[TodoTask], DietMapCContext]] = semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val RORMapEncoder: Encoder[RORMap.State[String, LastWriterWins.Embedded[TodoTask], DietMapCContext]] = semiauto.deriveEncoder: @scala.annotation.nowarn

  case class TodoRes(div: TypedTag[Div], tasklist: Signal[RORMap[String, LastWriterWins.Embedded[TodoTask], DietMapCContext]])

  @scala.annotation.nowarn // Auto-application to `()`
  def getContents(): TodoRes = {

    val todoInputTag: JsDom.TypedTag[Input] = input(
      id := "newtodo",
      `class` := "new-todo",
      placeholder := "What needs to be done?",
      autofocus := "autofocus"
    )

    val (createTodo, todoInputField) = inputFieldHandler(todoInputTag, onchange)

    val removeAll =  Events.fromCallback[UIEvent](cb => button("remove all done todos", onclick := cb))

    val toggleAll = Events.fromCallback[UIEvent] { cb =>
      input(id := "toggle-all", name := "toggle-all", `class` := "toggle-all", `type` := "checkbox", onchange := cb)
    }

    val myID = ThreadLocalRandom.current().nextLong().toHexString

    implicit val lattice: UIJDLattice[TodoTask] = AtomicUIJDLattice[TodoTask]

    def mapToList(m: RORMap[String, LastWriterWins.Embedded[TodoTask], DietMapCContext]): List[Option[TodoTask]] =
      m.queryAllEntries(LastWriterWinsCRDT.read).toList

    val deltaEvt: Evt[Delta[RORMap.State[String, LastWriterWins.Embedded[TodoTask], DietMapCContext]]] = Evt()

    case class ViewDataPair(view: Map[String, TodoTaskView], data: RORMap[String, LastWriterWins.Embedded[TodoTask], DietMapCContext])

    val initial = ViewDataPair(Map.empty[String, TodoTaskView], RORMap[String, LastWriterWins.Embedded[TodoTask], DietMapCContext](myID))

    val dataPlusUI = Events.foldAll(initial) { p =>
      Seq(
        createTodo >> { str =>
          val task = TodoTask(desc = str)
          val newMap = p.data.mutateKey(task.id, LastWriterWinsCRDT.write(task))
          println(s"Before createTodo: ${mapToList(p.data)}")
          println(s"After createTodo: ${mapToList(newMap)}")
          val ui = TodoTaskView.fromTask(task)
          ViewDataPair(p.view + (task.id -> ui), newMap)
        },
        removeAll.event >> { _ =>
          println("processing removeAll")

          val doneIDs = p.data.queryAllEntries(LastWriterWinsCRDT.read).collect {
            case Some(TodoTask(id, _, true)) => id
          }

          ViewDataPair(p.view.removedAll(doneIDs), p.data.removeAll(doneIDs))
        },
        EventSeqOps(p.view.values.toList.map(_.removeEvt)) >> { id =>
          println(s"removing task with id $id")
          ViewDataPair(p.view.removed(id), p.data.remove(id))
        },
        EventSeqOps(p.view.values.toList.map(_.writeEvt)) >> { task =>
          println(s"writing task $task")
          val newMap = p.data.mutateKey(task.id, LastWriterWinsCRDT.write(task))
          val ui = TodoTaskView.fromTask(task)
          ViewDataPair(p.view.updated(task.id, ui), newMap)
        },
        deltaEvt >> { delta =>
          val newMap = p.data.applyDelta(delta)

          val oldTasks = p.data.queryAllEntries(LastWriterWinsCRDT.read).toList.flatten
          val newTasks = newMap.queryAllEntries(LastWriterWinsCRDT.read).toList.flatten

          val uiMap = newTasks.map { t =>
            if (oldTasks.contains(t)) t.id -> p.view(t.id)
            else t.id -> TodoTaskView.fromTask(t)
          }.toMap

          ViewDataPair(uiMap, newMap)
        }
      )
    }

    val taskMap = dataPlusUI.map(_.data)

    dataPlusUI observe { p =>
      println(s"observed change on dataPlusUI: ${mapToList(p.data)}, view: ${p.view}")
    }

    LociDist.distributeDeltaCRDT(taskMap, deltaEvt, Todolist.registry)(Binding("taskmap"): @scala.annotation.nowarn)

    val tasks = dataPlusUI.map {
      _.data.queryAllEntries(
        v => LastWriterWinsCRDT.read[TodoTask, DietMapCContext](implicitly)(v)
      ).toList.flatten
    }

    val listItems = dataPlusUI.map(_.view.values.toList.map(ui => ui.tag))

    listItems observe { l =>
      println(s"listItems changed, new length: ${l.length}")
    }

    val content = div(
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

    TodoRes(content, taskMap)
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
