package lofi_acl.example.monotonic_acl

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.layout.VBox

object TravelPlannerAppMonotonic extends TravelPlannerApp

class TravelPlannerApp extends JFXApp3 {
  var controller: Option[TravelPlannerController] = None

  override def start(): Unit = {
    val createNewDocumentButton = new Button {
      text = "Create new travel plan"
      onAction = ev => {
        controller = Some(TravelPlannerController.forNewDocument)
      }
    }

    val remoteDocumentUriTextField = new TextField {
      promptText = "user@host:port"
      // hgrow = Priority.Always
    }

    val joinDocumentButton = new Button {
      text = "Join travel document"
      onAction = ev => {
        controller = Some(TravelPlannerController.forExistingDocument(remoteDocumentUriTextField.getText))
      }
    }

    stage = new JFXApp3.PrimaryStage {
      title.value = s"App with replicaId: ???"
      scene = new Scene {
        content = new VBox {
          children = Seq(
            createNewDocumentButton,
            remoteDocumentUriTextField,
            joinDocumentButton
          )
        }
      }
    }
  }

  override def stopApp(): Unit = {
    controller.foreach(_.shutdown())
  }
}
