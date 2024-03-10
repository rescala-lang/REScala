package reactives.extra

import org.scalajs.dom
import org.scalajs.dom.html.Input
import org.scalajs.dom.{Element, KeyboardEvent, MutationObserver, Node, Range, document}
import reactives.core.{CreationTicket, Disconnectable, DynamicScope, PlanTransactionScope, Scheduler, Tracing}
import reactives.operator.Interface
import reactives.structure.RExceptions.ObservedException
import reactives.structure.{Observe, Pulse}
import reactives.operator.*

import scala.annotation.targetName
import scala.scalajs.js

object Tags extends Tags(true)

class Tags(val addDebuggingIds: Boolean) {

  trait RangeSplice[-T]:
    def splice(anchor: dom.Element, range: dom.Range, value: T): Unit
  object RangeSplice:
    given elem: RangeSplice[dom.Element] with {
      override def splice(anchor: dom.Element, range: Range, value: dom.Element) =
        range.insertNode(value)
    }
    given many[T](using other: RangeSplice[T]): RangeSplice[Seq[T]] with {
      override def splice(anchor: dom.Element, range: Range, value: Seq[T]) =
        value.reverseIterator.foreach(v => other.splice(anchor, range, v))
    }
    given string: RangeSplice[String] with {
      override def splice(anchor: dom.Element, range: Range, value: String) =
        anchor.textContent = value
    }

  extension (anchor: dom.Element)
    def reattach[T](signal: Signal[T])(using
        splicer: RangeSplice[T],
        creationTicket: CreationTicket[Interface.State]
    ): anchor.type = {
      val range = document.createRange()
      Observe.strong(signal, true) {
        tagObserver(anchor, signal) { v =>
          if range.commonAncestorContainer != anchor then
            range.selectNodeContents(anchor)
            range.collapse(toStart = false)
          range.deleteContents()
          splicer.splice(anchor, range, v)
        }
      }
      anchor
    }

  extension (input: Input)
    def inputEntered(using
        creationTicket: CreationTicket[Interface.State],
        scheduler: PlanTransactionScope[Interface.State]
    ): Event[String] = {
      val handler: Event.CBR[KeyboardEvent, Unit] = Event.fromCallback(input.onkeyup = Event.handle(_))

      handler.event
        .map { (e: KeyboardEvent) =>
          if e.key == "Enter" then
            val res = input.value.trim
            if res.nonEmpty then
              e.preventDefault()
              input.value = ""
              Some(res)
            else None
          else None
        }.flatten
    }

  def isInDocument(element: Element): Boolean = {
    js.Dynamic.global.document.contains(element).asInstanceOf[Boolean]
  }

  /* This only returns true the second time it is called to prevent observers to directly trigger */
  def isInDocumentHack(elem: dom.Element): Any => Boolean = {
    var second = false
    _ => {
      if (second) {
        !isInDocument(elem)
      } else {
        second = true
        false
      }
    }
  }

  def tagObserver[A](
      parent: dom.Element,
      rendered: Signal[A]
  )(fun: A => Unit)(reevalVal: Pulse[A]): Observe.ObserveInteract =
    new Observe.ObserveInteract {
      override def checkExceptionAndRemoval(): Boolean = {
        reevalVal match {
          case Pulse.empty(_) | Pulse.NoChange => false
          case Pulse.Exceptional(f) =>
            throw ObservedException(rendered, s"signal tag attached to $parent observed", f)
          case Pulse.Value(v) =>
            isInDocumentHack(parent)(v)
        }
      }

      override def execute(): Unit =
        reevalVal match {
          case Pulse.empty(_) | Pulse.NoChange => ()
          case Pulse.Value(v) =>
            fun(v)
          case Pulse.Exceptional(f) =>
            throw new IllegalStateException("should have aborted earlier", f)
        }
    }

}
