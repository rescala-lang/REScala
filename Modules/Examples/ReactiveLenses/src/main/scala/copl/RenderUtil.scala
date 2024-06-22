package copl

import org.scalajs.dom.UIEvent
import org.scalajs.dom.html.{Input, Select}
import reactives.operator.*
import scalatags.JsDom.all.*
import scalatags.JsDom.{Attr, TypedTag}

object RenderUtil {
  def inputFieldHandler(tag: TypedTag[Input], attr: Attr, clear: Boolean = true): (Event[String], Input) = {
    val handler               = Event.fromCallback(tag(attr := Event.handle[UIEvent]))
    val todoInputField: Input = handler.data.render

    // observer to prevent form submit and empty content
    handler.event.observe { (e: UIEvent) =>
      e.preventDefault()
      if clear then todoInputField.value = ""
    }

    // note that the accessed value is NOT a reactive, there is a name clash with the JS library :-)
    val inputFieldText = handler.event.map { _ => todoInputField.value.trim }

    (inputFieldText, todoInputField)
  }

  def dropDownHandler(tag: TypedTag[Select], attr: Attr, clear: Boolean = true): (Event[String], Select) = {
    val handler                = Event.fromCallback(tag(attr := Event.handle[UIEvent]))
    val todoInputField: Select = handler.data.render

//    // observer to prevent form submit and empty content
//    handler.event.observe { (e: UIEvent) =>
//      e.preventDefault()
//      if (clear) todoInputField.value = ""
//    }
//
//    // note that the accessed value is NOT a reactive, there is a name clash with the JS library :-)
    val inputFieldText = handler.event.map { _ => todoInputField.value.trim }

    (inputFieldText, todoInputField)
  }

  def setInputDisplay(in: Input, text: String): Unit = {
    in.value = text
  }
}
