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
import rescala.extra.lattices.delta.crdt.RRGA._
import rescala.extra.lattices.delta.crdt.{Elem, GOList, GOListNode, RGANode, RRGA}
import rescala.extra.lattices.delta.{Causal, Delta, Dot, TimedVal}
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import scalatags.JsDom.{Attr, TypedTag}

import java.util.concurrent.ThreadLocalRandom

class TodoApp() {

  implicit val transmittableTaskList: IdenticallyTransmittable[RRGA.State[TodoTask, DietMapCContext]] =
    IdenticallyTransmittable()

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

  implicit val GOListNodeDecoder: Decoder[GOListNode[TimedVal[Dot]]] = semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val GOListNodeEncoder: Encoder[GOListNode[TimedVal[Dot]]] = semiauto.deriveEncoder: @scala.annotation.nowarn

  implicit val DotTimedValDecoder: Decoder[TimedVal[Dot]] = semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val DotTimedValEncoder: Encoder[TimedVal[Dot]] = semiauto.deriveEncoder: @scala.annotation.nowarn

  implicit val ElemDecoder: Decoder[Elem[TimedVal[Dot]]] = semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val ElemEncoder: Encoder[Elem[TimedVal[Dot]]] = semiauto.deriveEncoder: @scala.annotation.nowarn

  implicit val GOListDecoder: Decoder[GOList.State[Dot]] =
    Decoder.decodeList[(GOListNode[TimedVal[Dot]], Elem[TimedVal[Dot]])].map(_.toMap)
  implicit val GOListEncoder: Encoder[GOList.State[Dot]] =
    Encoder.encodeList[(GOListNode[TimedVal[Dot]], Elem[TimedVal[Dot]])].contramap(_.toList)

  implicit val TodoTaskTimedValDecoder: Decoder[TimedVal[TodoTask]] = semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val TodoTaskTimedValEncoder: Encoder[TimedVal[TodoTask]] = semiauto.deriveEncoder: @scala.annotation.nowarn

  implicit val RGANodeDecoder: Decoder[RGANode[TodoTask]] = semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val RGANodeEncoder: Encoder[RGANode[TodoTask]] = semiauto.deriveEncoder: @scala.annotation.nowarn

  implicit val DotFunDecoder: Decoder[DotFun[RGANode[TodoTask]]] =
    Decoder.decodeList[(Dot, RGANode[TodoTask])].map(_.toMap)
  implicit val DotFunEncoder: Encoder[DotFun[RGANode[TodoTask]]] =
    Encoder.encodeList[(Dot, RGANode[TodoTask])].contramap(_.toList)

  implicit val CausalDecoder: Decoder[Causal[DotFun[RGANode[TodoTask]], DietMapCContext]] =
    semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val CausalEncoder: Encoder[Causal[DotFun[RGANode[TodoTask]], DietMapCContext]] =
    semiauto.deriveEncoder: @scala.annotation.nowarn

  implicit val RRGADecoder: Decoder[RRGA.State[TodoTask, DietMapCContext]] =
    semiauto.deriveDecoder: @scala.annotation.nowarn
  implicit val RRGAEncoder: Encoder[RRGA.State[TodoTask, DietMapCContext]] =
    semiauto.deriveEncoder: @scala.annotation.nowarn

  case class TodoRes(div: TypedTag[Div], tasklist: Signal[RRGA[TodoTask, DietMapCContext]])
  case class ViewDataPair(view: Map[String, TodoTaskView], data: RRGA[TodoTask, DietMapCContext])

  @scala.annotation.nowarn // Auto-application to `()`
  def getContents(): TodoRes = {

    val todoInputTag: JsDom.TypedTag[Input] = input(
      id := "newtodo",
      `class` := "new-todo",
      placeholder := "What needs to be done?",
      autofocus := "autofocus"
    )

    val (createTodo, todoInputField) = inputFieldHandler(todoInputTag, onchange)

    val removeAll = Events.fromCallback[UIEvent](cb => button("remove all done todos", onclick := cb))

    val toggleAll = Events.fromCallback[UIEvent] { cb =>
      input(id := "toggle-all", name := "toggle-all", `class` := "toggle-all", `type` := "checkbox", onchange := cb)
    }

    val myID = ThreadLocalRandom.current().nextLong().toHexString

    val deltaEvt: Evt[Delta[RRGA.State[TodoTask, DietMapCContext]]] = Evt()

    val initial = ViewDataPair(Map.empty[String, TodoTaskView], RRGA[TodoTask, DietMapCContext](myID))

    val dataPlusUI = Events.foldAll(initial) { p =>
      Seq(
        createTodo >> { str =>
          val task    = TodoTask(desc = str)
          val newList = p.data.insert(0, task)
          val ui      = TodoTaskView.fromTask(task)

          ViewDataPair(p.view + (task.id -> ui), newList)
        },
        removeAll.event >> { _ =>
          val doneIDs = p.data.toList.collect {
            case TodoTask(id, _, true) => id
          }

          ViewDataPair(p.view.removedAll(doneIDs), p.data.deleteBy(_.done))
        },
        EventSeqOps(p.view.values.toList.map(_.removeEvt)) >> { id =>
          ViewDataPair(p.view.removed(id), p.data.deleteBy(_.id == id))
        },
        EventSeqOps(p.view.values.toList.map(_.writeEvt)) >> { task =>
          val newList = p.data.updateBy(_.id == task.id, task)

          val ui = TodoTaskView.fromTask(task)

          ViewDataPair(p.view.updated(task.id, ui), newList)
        },
        deltaEvt >> { delta =>
          val newList = p.data.applyDelta(delta)

          val oldTasks = p.data.toList
          val newTasks = newList.toList

          val uiMap = newTasks.map { t =>
            if (oldTasks.contains(t)) t.id -> p.view(t.id)
            else t.id                      -> TodoTaskView.fromTask(t)
          }.toMap

          ViewDataPair(uiMap, newList)
        }
      )
    }

    val taskList = dataPlusUI.map(_.data)

    LociDist.distributeDeltaCRDT(taskList, deltaEvt, Todolist.registry)(Binding("tasklist"): @scala.annotation.nowarn)

    val tasks = taskList.map(_.toList)

    val listItems = dataPlusUI.map { p =>
      p.data.toList.map(t => p.view(t.id).tag)
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

    TodoRes(content, taskList)
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
