package lofi_acl.example.monotonic_acl

import scalafx.application.Platform
import scalafx.beans.property.{BooleanProperty, StringProperty}
import scalafx.collections.transformation.SortedBuffer
import scalafx.scene.control.{ListView, TextField}
import scalafx.stage.Stage

import scala.concurrent.ExecutionContext.global

class MainSceneViewModel {
  @volatile private var model: TravelPlanModel = scala.compiletime.uninitialized

  val documentIsOpen: BooleanProperty = new BooleanProperty()
  documentIsOpen.value = false

  val inviteString: StringProperty = new StringProperty()

  // Travel Plan:
  val titleTextField: TextField = TextField()

  val bucketListView: ListView[String] = ListView[String]()

  val expenseView: ListView[String] = ListView[String]()

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

  def createNewDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    require(model == null)
    // TODO: Check if running this directly on the GUI Thread has better UX (glitches). Same on join.
    global.execute { () =>
      model = TravelPlanModel.createNewDocument
      Platform.runLater(init())
    }
  }

  def joinDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    require(model == null)
    global.execute { () =>
      model = TravelPlanModel.joinDocument(inviteString.value)
      Platform.runLater(init())
    }
  }

  private def init(): Unit = {
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
    bucketListView.cellFactory = { (_: Any) => new BucketListEntryListCell(model) }
    bucketListView.items = SortedBuffer(model.bucketListIdList)
    expenseView.cellFactory = { (_: Any) => new ExpenseListEntryListCell(model) }
    expenseView.items = SortedBuffer(model.expenseIdList)
  }
}
