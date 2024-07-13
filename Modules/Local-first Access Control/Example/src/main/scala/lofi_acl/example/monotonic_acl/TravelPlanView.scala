package lofi_acl.example.monotonic_acl

import javafx.geometry.Pos
import javafx.scene.control as jfxsc
import javafx.util.Callback
import scalafx.application.Platform
import scalafx.collections.transformation.SortedBuffer
import scalafx.scene.control.{Button, ListView, TextField}
import scalafx.scene.layout.GridPane
import scalafx.stage.Stage

import scala.concurrent.ExecutionContext.global

class TravelPlanView(viewModel: TravelPlanViewModel) extends GridPane {
  private val createInvitationButton = Button()
  createInvitationButton.text = "Share"
  createInvitationButton.onAction = ev => viewModel.createInviteButtonPressed()

  val bucketListView: ListView[String] = ListView[String]()
  bucketListView.cellFactory = viewModel.bucketListCellFactory
  bucketListView.items = viewModel.bucketListIds
  bucketListView.prefHeight = 200

  val expenseView: ListView[String] = ListView[String]()
  expenseView.cellFactory = viewModel.expenseViewCellFactory
  expenseView.items = viewModel.expenseIds
  expenseView.prefHeight = 200

  add(viewModel.titleTextField, 0, 0)
  viewModel.titleTextField.setAlignment(Pos.CENTER)
  add(createInvitationButton, 1, 0)
  add(bucketListView, 0, 1, 2, 1)
  add(expenseView, 0, 2, 2, 1)

  Platform.runLater(createInvitationButton.requestFocus())
}

class TravelPlanViewModel(model: TravelPlanModel) extends GridPane {
  val bucketListIds: SortedBuffer[String] = SortedBuffer(model.bucketListIdList)
  val expenseIds: SortedBuffer[String]    = SortedBuffer(model.expenseIdList)

  // Travel Plan:
  val titleTextField: TextField = TextField()

  titleTextField.text = model.title.get()
  model.title.onChange((op, oldVal, newVal) =>
    if !titleTextField.isFocused then
      titleTextField.text = model.state.title.read
  )

  titleTextField.text.onChange((op, oldVal, newVal) =>
    if titleTextField.isFocused then
      model.changeTitle(newVal)
  )
  titleTextField.focused.onChange((op, wasFocused, isNowFocused) =>
    if !isNowFocused then
      titleTextField.text = model.title.get()
  )

  def bucketListCellFactory: Callback[jfxsc.ListView[String], jfxsc.ListCell[String]] = { (_: Any) =>
    new BucketListEntryListCell(model)
  }

  def expenseViewCellFactory: Callback[jfxsc.ListView[String], jfxsc.ListCell[String]] = { (_: Any) =>
    new ExpenseListEntryListCell(model)
  }

  def createInviteButtonPressed(): Unit = {
    global.execute { () =>
      val invitation = model.createInvitation
      Platform.runLater(() => {
        val scene = InvitationDialogScene(invitation, model)
        val stage = new Stage()
        stage.setTitle("New Invite")
        stage.scene = scene
        stage.show()
      })
    }
  }
}
