package todo

import java.util.concurrent.ThreadLocalRandom

import org.scalajs.dom
import org.scalajs.dom.html.{Input, LI}
import org.scalajs.dom.{KeyboardEvent, UIEvent, document}
import rescala.core.{ReSerializable, Scheduler}
import rescala.debuggable.ChromeDebuggerInterface
import rescala.restoration.{LocalStorageStore, ReStoringStruct}
import rescala.restoration.ReCirce.recirce
import rescala.Tags._
import scalatags.JsDom
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section

import scala.scalajs.js.annotation.JSExportTopLevel

object TodoMVC {

  implicit val storingEngine: LocalStorageStore = new LocalStorageStore()
  import storingEngine._


//  implicit val taskDecoder: io.circe.Decoder[Task] = io.circe.Decoder.decodeTuple2[Var[String], Var[Boolean]].map { case (desc, done) =>
//    new Task(desc, done)
//  }
//  implicit val taskEncoder: io.circe.Encoder[Task] = io.circe.Encoder.encodeTuple2[Var[String], Var[Boolean]].contramap{ t =>
//    (t.desc, t.done)
//  }

//  class Task(val desc : Var[String], val done : Signal[Boolean]) {
//    val editing = Var(false)(ReSerializable.doNotSerialize, implicitly)
//  }
//  object Task {
//    def apply(desc: String, done: Boolean) = {
//      val rn = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"
//      val descV = Var(desc)(implicitly, rn.toString)
//      val doneV = toggleAll.fold(done)((v, _) => !v)(rn.toString + "b", implicitly)
//      new Task(descV, doneV)
//    }
//  }

  class Taskres(val item: TypedTag[LI], val done: Signal[Boolean], val removeClick: Event[Taskres])

  def maketask[__](str: String, toggleAll: Event[__]): Taskres = {
    val rn = s"Task(${ThreadLocalRandom.current().nextLong().toHexString})"


    val (edittext, edittextInput) = Events.fromCallback[UIEvent]{ inputChange =>
      input(
        `class` := "edit", `type` := "text",
        onchange := inputChange, onblur := inputChange,
        onkeypress := { e: KeyboardEvent =>
          if (e.keyCode == 13) { // 13 = enter key
            e.preventDefault() // TODO somehow app breaks, if we listen to enter...?
          }
        })
    }

    val edittextStr = edittext
                      .map { e: UIEvent =>
                        val myinput = e.target.asInstanceOf[Input]
                        myinput.value.trim
                      }

    val descV = edittextStr
                .fold(str) { (_, uie) => uie }(rn.toString, implicitly)

    val editingV = edittextStr.fold(false)((_, _) => false)


    val (doneClick, doneClickModifier) = Events.fromCallback[UIEvent](onchange := _)

    val doneV = (toggleAll || doneClick).fold(false)((v, _) => !v)(rn.toString + "b", implicitly)


    val (removeClick, removeTaskButton) = addHandler[UIEvent](button(`class` := "destroy"), onclick)



    val listItem = li(
      `class` := Signal (
        (if (doneV.value) "completed " else " ")
        +(if (editingV.value) "editing " else "no-editing ")
      ),

      div(
        `class`:="view",

        ondblclick:= { e: UIEvent =>
//          tasks.readValueOnce.foreach( tt => tt.editing.set(t==tt) )
        },
        Signal {
          input(`class` := "toggle", `type` := "checkbox", doneClickModifier,
                if (doneV.value) checked else "" )}.asModifier,
        label(descV.map(stringFrag).asModifier),
        removeTaskButton
      ),


      edittextInput(value := descV)
    )

    lazy val res: Taskres = new Taskres(listItem, doneV, removeClick.map(_ => res))
    res
  }

  @JSExportTopLevel("todo.TodoMVC.main")
  def main(): Unit = {


    ChromeDebuggerInterface.setup(storingEngine)

    val todoInputTag: JsDom.TypedTag[Input] = input(
      id := "newtodo",
      `class` := "new-todo",
      placeholder := "What needs to be done?",
      autofocus := "autofocus")

    val (createTodo, todoInputField) = createHandler(todoInputTag, onchange)


    val (toggleAll, toggleAllTag) = addHandler[UIEvent](
      input(id := "toggle-all", name := "toggle-all", `class` := "toggle-all", `type` := "checkbox"),
      onchange)


    val innerTasks = List(
      maketask("walk the dog", toggleAll),
      maketask("get milk", toggleAll),
      maketask("get coffee", toggleAll)
    )

    val createTask = createTodo.map { str => maketask(str, toggleAll) }


    val (removeAllClick, removeAllButton) = addHandler[UIEvent](button("remove all done todos"), onclick)

    val tasks = Events.foldAll(innerTasks) { tasks =>
      Seq(
        createTask >> {_ :: tasks},
        removeAllClick >>> { dt => _ => tasks.filterNot(t => dt.depend(t.done)) },
        tasks.map(_.removeClick) >> { t => tasks.filter(_ != t) }
      )
    }(ReSerializable.doNotSerialize, "tasklist")



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
        toggleAllTag,
        label(`for` := "toggle-all", "Mark all as complete"),
        tasks.map(l =>
                    ul(
                      `class` := "todo-list",
                      l.map(_.item))).asModifier
      ),
      div(
        `class`:="footer",
        `style`:= Signal { if(tasks().isEmpty) "display:none" else "" },

        Signal.dynamic {
          val remainingTasks = tasks().filter(!_.done())
          span(
            `class`:="todo-count",
            strong("" + remainingTasks.size),
            span(if (remainingTasks.size == 1)
              " item left" else " items left")
          )
        }.asModifier,

        removeAllButton(`class` := Signal.dynamic {
          "clear-completed" +
          (if (!tasks().exists(t => t.done())) " hidden" else "")
        })
      )
    )

    document.body.replaceChild(content.render, document.body.firstElementChild)

    ChromeDebuggerInterface.finishedLoading()
  }

  def createHandler(tag: TypedTag[Input], attr: Attr): (Event[String], Input) = {
    val (createTodo: storingEngine.Event[UIEvent], todoInputField1: TypedTag[Input]) = addHandler[UIEvent][Input](tag, attr)

    val todoInputField: Input = todoInputField1.render


    (createTodo.map { e: UIEvent =>
      e.preventDefault()
      val res = todoInputField.value.trim
      todoInputField.value = ""
      res
    }, todoInputField)
  }


  final class addHandlerT[Ev](val p: Unit) extends AnyVal {
    def apply[T <: dom.Element](tag: TypedTag[T], attr: Attr)
                               (implicit ct: CreationTicket, s: Scheduler[ReStoringStruct]): (Event[Ev], TypedTag[T]) =
      Events.fromCallback[Ev](cb => tag(attr := cb))(ct, s)
  }
  def addHandler[Ev]: addHandlerT[Ev] = new addHandlerT[Ev](())
}
