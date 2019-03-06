package todo

import java.util.concurrent.ThreadLocalRandom

import io.circe.generic.semiauto
import io.circe.{Decoder, Encoder}
import loci.communicator.experimental.webrtc.WebRTC.ConnectorFactory
import org.scalajs.dom.html.{Input, LI}
import org.scalajs.dom.{UIEvent, document}
import rescala.Tags._
import rescala.core.ReSerializable
import rescala.debuggable.ChromeDebuggerInterface
import rescala.restoration.LocalStorageStore
import rescala.restoration.ReCirce.recirce
import scalatags.JsDom
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section

import scala.Function.const
import scala.scalajs.js.timers.setTimeout
import loci.communicator.experimental.webrtc._
import loci.registry.{Binding, Registry}
import loci.serializer.circe._
import loci.transmitter.RemoteRef
import io.circe.syntax._


import io.circe.generic.auto._

import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue


object TodoMVC {

  implicit val storingEngine: LocalStorageStore = new LocalStorageStore()
  import storingEngine._

  val toggleAll = Events.fromCallback[UIEvent] { cb =>
    input(id := "toggle-all", name := "toggle-all", `class` := "toggle-all",
          `type` := "checkbox", onchange := cb)
  }

  case class TaskData(desc: String, done: Boolean = false) {
    def toggle(): TaskData = copy(done = !done)
    def edit(str: String) = copy(desc = str)
  }

  implicit val taskDataDecoder: Decoder[TaskData] = semiauto.deriveDecoder
  implicit val taskDataEncoder: Encoder[TaskData] = semiauto.deriveEncoder

  implicit val taskDecoder: Decoder[Taskref] =
    Decoder.decodeTuple2[String, TaskData].map{ case (s, td) => maketask(td, s) }
  implicit val taskEncoder: Encoder[Taskref] =
    Encoder.encodeTuple2[String, TaskData].contramap[Taskref](tr => (tr.id, tr.initial))

  class Taskref(val id: String,
                val listItem: TypedTag[LI],
                val contents: Signal[TaskData],
                val initial: TaskData,
                val removeClick: Event[String])

  def maketask(initial: TaskData,
               randomName: String = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})")
  : Taskref = {


    val edittext = Events.fromCallback[UIEvent]{ inputChange =>
      input(`class` := "edit", `type` := "text", onchange := inputChange, onblur := inputChange)
    }

    val edittextStr = edittext.event.map { e: UIEvent =>
      val myinput = e.target.asInstanceOf[Input]
      myinput.value.trim
    }


    val editDiv = Events.fromCallback[UIEvent] { cb =>
      div(`class` := "view", ondblclick := cb)
    }

    val changeEditing = (edittextStr map const(false)) || (editDiv.event map const(true))
    val editingV =  changeEditing.latest(init = false)(ReSerializable.doNotSerialize, implicitly)

    val doneClick = Events.fromCallback[UIEvent](onchange := _)

    val doneEv = toggleAll.event || doneClick.event

    val contents = Events.foldAll(initial)(current => Seq(
      doneEv >> const(current.toggle),
      edittextStr >> current.edit
    ))(implicitly, randomName)


    val removeButton =
      Events.fromCallback[UIEvent](cb => button(`class` := "destroy", onclick := cb))

    val editInput = edittext.value(value := contents.map(_.desc)).render
    editDiv.event.observe(_ => setTimeout(0){editInput.focus()})

    val listItem = li(
      `class` := Signal(
        (if (contents.value.done) "completed " else "")
        + (if (editingV.value) "editing " else "no-editing ")
      ),

      editDiv.value(
        input(`class` := "toggle", `type` := "checkbox", doneClick.value,
              checked := contents.map(c => if (c.done) Some(checked.v) else None))(checked := false),
        label(contents.map(c => stringFrag(c.desc)).asModifier),
        removeButton.value
      ),
      editInput
    )

    new Taskref(randomName, listItem, contents, initial, removeButton.event.map(_ => randomName))
  }


  val registry = new Registry
  val crdtDescriptions = Binding[TaskData => String]("taskData")

  def main(args: Array[String]): Unit = {

    ChromeDebuggerInterface.setup(storingEngine)

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

    val remote = Events.fromCallback[TaskData](cb => registry.bind(crdtDescriptions){td =>
      cb(td)
      "success"
    })

    val creations = createTodo.map(str => TaskData(str)) || remote.event

    val createTask = creations.map { td => maketask(td) }


    val tasks = Events.foldAll(innerTasks) { tasks =>
      Seq(
        createTask >> {_ :: tasks},
        removeAll.event >>> { dt => _ => tasks.filterNot(t => dt.depend(t.contents).done) },
        tasks.map(_.removeClick) >> { t => tasks.filter(_.id != t) }
      )
    }(implicitly, "tasklist")


    val content = div(
      `class`:="todoapp",
      header(
        `class`:="header",
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
        `class`:="footer",
        `style`:= Signal { if(tasks().isEmpty) "display:none" else "" },

        Signal.dynamic {
          val remainingTasks = tasks().filter(!_.contents.value.done)
          span(
            `class`:="todo-count",
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


    document.body.replaceChild(content.render, document.body.firstElementChild)
    document.body.appendChild(webrtchandlingArea.render)

    ChromeDebuggerInterface.finishedLoading()
  }






  def webrtchandlingArea: Tag = {

    val renderedTa = textarea().render
    val renderedPre = pre().render

    var pn: Option[PendingNonesense] = None

    val hb = button("host", onclick := { uie: UIEvent =>
      val res = webrtcIntermediate(WebRTC.offer())
      res.session.foreach(s => renderedPre.textContent = s.asJson.noSpaces)
      pn = Some(res)
      val remoteFut = registry.connect(res.connector)
      remoteFut.foreach { remote =>
        registry.lookup(crdtDescriptions, remote)(TaskData("connected!!!!"))
      }
    })


    val cb = button("connect", onclick := { uie: UIEvent =>
      val cs = io.circe.parser.decode[WebRTC.CompleteSession](renderedTa.value).right.get
      (pn match {
        case None     => // we are client
          val res = webrtcIntermediate(WebRTC.answer())
          res.session.foreach(s => renderedPre.textContent = s.asJson.noSpaces)
          registry.connect(res.connector)
          res.connector
        case Some(ss) => // we are server
          ss.connector
      }) set cs
    })

    section(hb, cb, renderedPre, renderedTa)


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


  case class PendingNonesense(connector: WebRTC.Connector,
                         session: Future[WebRTC.CompleteSession])

  def webrtcIntermediate(cf: ConnectorFactory) = {
    val p = Promise[WebRTC.CompleteSession]()
    val answer = cf complete(p.success)
    PendingNonesense(answer, p.future)
  }


}
