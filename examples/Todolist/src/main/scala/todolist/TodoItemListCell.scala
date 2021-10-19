package de.ckuessner
package todolist


import javafx.scene.control.ListCell
import scalafx.Includes._
import scalafx.beans.binding.Bindings
import scalafx.beans.property.ObjectProperty
import scalafx.beans.value.ObservableValue
import scalafx.geometry.Pos
import scalafx.scene.control.{CheckBox, TextField}
import scalafx.scene.layout.{HBox, Priority}

import java.lang
import java.util.UUID

class TodoItemListCell extends ListCell[UUID] {
  var todoProperty: Option[ObjectProperty[TodoEntry]] = None

  private val textField = new TextField {
    hgrow = Priority.Always
  }
  textField.text.onChange { (source: ObservableValue[String, String], oldVal, newVal) =>
    val uuid = getItem
    todoProperty match {
      case Some(property) => TodoListController.changeTodo(uuid, property.value.copy(description = newVal))
      case None => Console.err.println(s"TodoItemListCell: Entry $uuid not present in Controller")
    }
  }

  private val checkBox = new CheckBox()
  checkBox.selectedProperty.onChange { (observable: ObservableValue[Boolean, java.lang.Boolean], oldVal, newVal) =>
    val uuid = getItem
    todoProperty match {
      case Some(property) => TodoListController.changeTodo(uuid, property.value.copy(completed = newVal))
      case None => Console.err.println(s"TodoItemListCell: Entry $uuid not present in Controller")
    }
  }

  private val rootContainer = new HBox {
    children = List(checkBox, textField)
    alignment = Pos.CenterLeft
  }

  override def updateItem(uuid: UUID, empty: Boolean): Unit = {
    super.updateItem(uuid, empty)
    if (empty || uuid == null) {
      setText(null)
      setGraphic(null)
    } else {
      setGraphic(rootContainer)
      todoProperty = TodoListController.getTodo(uuid)
      if (todoProperty.isDefined) {
        textField.setText(todoProperty.get.value.description)
        checkBox.setSelected(todoProperty.get.value.completed)

        val descriptionBinding = Bindings.createStringBinding(() => todoProperty.get.value.description, todoProperty.get)
        descriptionBinding.onChange((observable: ObservableValue[String, String], oldVal: String, newVal: String) =>
          textField.setText(newVal)
        )

        val completedBinding = Bindings.createBooleanBinding(() => todoProperty.get.value.completed, todoProperty.get)
        completedBinding.onChange((observable, oldVal, newVal: lang.Boolean) =>
          checkBox.selected = newVal
        )
      }
    }
  }
}
