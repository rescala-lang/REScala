package lofi_acl.example.monotonic_acl

import javafx.scene.control as jfxsc
import javafx.util.Callback
import scalafx.application.Platform
import scalafx.collections.transformation.SortedBuffer
import scalafx.scene.control.TextField
import scalafx.scene.layout.GridPane
import scalafx.stage.Stage

import scala.concurrent.ExecutionContext.global

class TravelPlanViewModel(model: TravelPlanModel) extends GridPane {
  val bucketListIds: SortedBuffer[String] = SortedBuffer(model.bucketListIdList, Ordering.String)
  val expenseIds: SortedBuffer[String]    = SortedBuffer(model.expenseIdList, Ordering.String)

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

  def createBucketListEntryButtonPressed(entryText: String): Unit = {
    global.execute { () =>
      model.addBucketListEntry(entryText)
    }
  }

  def addExpenseButtonPressed(entryText: String, amount: String): Unit = {
    global.execute { () =>
      model.addExpense(entryText, amount)
    }
  }
}
