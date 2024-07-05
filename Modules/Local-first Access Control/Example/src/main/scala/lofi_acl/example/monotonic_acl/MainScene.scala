package lofi_acl.example.monotonic_acl

import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.layout.{BorderPane, HBox, VBox}

class MainScene extends Scene {
  private val viewModel = new MainSceneViewModel()
  private val rootPane  = new BorderPane()

  private val createNewDocumentButton: Button = new Button("Create new Travel Plan Document")
  createNewDocumentButton.alignment = Pos.Center
  createNewDocumentButton.onAction = _ => viewModel.createNewDocumentButtonPressed()
  createNewDocumentButton.disable <== viewModel.documentIsOpen

  private val joinDocumentButton: Button = new Button("Join")
  joinDocumentButton.alignment = Pos.Center
  joinDocumentButton.onAction = _ => viewModel.joinDocumentButtonPressed()
  joinDocumentButton.disable <== viewModel.documentIsOpen

  private val remoteDocumentUriTextField = new TextField {
    promptText = "user@host:port"
    // hgrow = Priority.Always
  }
  remoteDocumentUriTextField.disable <== viewModel.documentIsOpen
  remoteDocumentUriTextField.text <==> viewModel.joinDocumentUri

  rootPane.center = VBox(
    createNewDocumentButton,
    HBox(
      remoteDocumentUriTextField,
      joinDocumentButton
    )
  )

  content = rootPane

  // TODO: Hook in the document view on document open
}
