package encfxtodo

import scalafx.Includes.*
import scalafx.application.{JFXApp3, Platform}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ListView, TextField}
import scalafx.scene.layout.{HBox, Priority, VBox}

import java.util.UUID

object TodoListAppPeerToPeer extends TodoListApp

class TodoListApp extends JFXApp3 {
  override def start(): Unit = {
    val todoListView = new ListView[UUID] {
      cellFactory = { (_: Any) => new TodoItemListCell() }
      items = TodoListController.observableUuidList
    }

    val newTodoTextField = new TextField {
      promptText = "Todo Entry"
      hgrow = Priority.Always
    }

    val addTodoButton = new Button {
      text = "+"
      onAction = () => {
        TodoListController.addTodo(TodoEntry(newTodoTextField.text.value))
        newTodoTextField.clear()
      }
    }

    val connectionTextField = new TextField {
      promptText = "replica@remote"
      hgrow = Priority.Always
    }

    val localAddressTextField = new TextField {
      text = TodoListController.connectionString
      editable = false
    }
    localAddressTextField.focusedProperty.addListener(_ =>
      Platform.runLater {
        if localAddressTextField.isFocused then {
          localAddressTextField.selectAll()
        }
      }
    )

    val addConnectionButton = new Button("Connect")
    addConnectionButton.onAction = () => {
      val connectionString = connectionTextField.getText
      if !connectionString.forall(Character.isWhitespace) then
        TodoListController.connect(connectionString)
    }

    stage = new JFXApp3.PrimaryStage {
      title.value = s"Encrdt Todolist (replica ${TodoListController.replicaId})"
      scene = new Scene {
        content = new VBox {
          children = Seq(
            todoListView,
            new HBox {
              children = Seq(newTodoTextField, addTodoButton)
            },
            localAddressTextField,
            new HBox {
              children = Seq(connectionTextField, addConnectionButton)
            },
            // new HBox {
            //  children = Seq(
            //    new Button {
            //      text = "Log state"
            //      onAction = () => Console.println(s"State: ${TodoListController.todos})")
            //    },
            //    new Button {
            //      text = "Log peers"
            //      onAction = () => Console.println(s"Peers: ${TodoListController.remoteAddresses})")
            //    }
            //  )
            // }
          )
        }
      }
    }
  }

  override def stopApp(): Unit = {
    TodoListController.stop()
  }
}
