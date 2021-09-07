package de.ckuessner
package todolist

import scalafx.Includes._
import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ListView, TextField}
import scalafx.scene.layout.{HBox, VBox}

import java.util.UUID

object TodoListApp extends JFXApp3 {
  override def start(): Unit = {
    val todoListView = new ListView[UUID] {
      cellFactory = { listView => new TodoItemListCell() }
      items = TodoListController.observableUuidList
    }

    val newTodoTextField = new TextField {
      promptText = "Todo Entry"
    }

    val addTodoButton = new Button {
      text = "Add"
      onAction = () =>{
        TodoListController.addTodo(TodoEntry(newTodoTextField.text.value))
        newTodoTextField.clear()
      }
    }

    val connectionTextField = new TextField {
      promptText = "replica@remote"
    }

    val localAddressTextField = new TextField {
      text = TodoListController.connectionString
      editable = false
    }

    val addConnectionButton = new Button("Connect")
    addConnectionButton.onAction = () => {
      val connectionString = connectionTextField.getText
      if (connectionString.isBlank) return
      TodoListController.connect(connectionString)
    }

    stage = new JFXApp3.PrimaryStage {
      title.value = s"Encrdt Todolist (replica ${TodoListController.replicaId})"
      scene = new Scene {
        content = new VBox {
          children = Seq(
            todoListView,
            new HBox {
              children = Seq(addTodoButton, newTodoTextField)
            },
            new HBox {
              children = Seq(addConnectionButton, connectionTextField)
            },
            localAddressTextField
          )
        }
      }
    }
  }

  override def stopApp(): Unit = {
    TodoListController.stop()
  }
}