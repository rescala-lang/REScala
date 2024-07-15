package lofi_acl.example.monotonic_acl

import javafx.geometry.Pos
import scalafx.application.Platform
import scalafx.scene.control.{Button, ListView, TextField, TextFormatter}
import scalafx.scene.layout.GridPane

class TravelPlanView(viewModel: TravelPlanViewModel) extends GridPane {
  private val shareButton = Button()
  shareButton.text = "Share"
  shareButton.onAction = ev => viewModel.createInviteButtonPressed()
  // jfxsl.GridPane.setFillWidth(shareButton, true)

  private val bucketListView: ListView[String] = ListView[String]()
  bucketListView.cellFactory = viewModel.bucketListCellFactory
  bucketListView.items = viewModel.bucketListIds
  bucketListView.prefHeight = 200
  private val addBucketListEntryTextField = TextField()
  addBucketListEntryTextField.promptText = "Bucket List Entry"
  private val addBucketListEntryButton = Button("Add Entry")
  addBucketListEntryButton.disable <== addBucketListEntryTextField.text.isEmpty
  addBucketListEntryButton.onAction = { ev =>
    val entryText = addBucketListEntryTextField.text.get()
    addBucketListEntryTextField.text.value = ""
    viewModel.createBucketListEntryButtonPressed(entryText)
  }

  private val expenseView: ListView[String] = ListView[String]()
  expenseView.cellFactory = viewModel.expenseViewCellFactory
  expenseView.items = viewModel.expenseIds
  expenseView.prefHeight = 200
  private val addExpenseEntryTextField  = TextField()
  private val addExpenseAmountTextField = TextField()
  addExpenseAmountTextField.text = "0.00 €"
  addExpenseAmountTextField.textFormatter = TextFormatter(ExpenseListEntryListCell.amountTextFilter)
  addExpenseAmountTextField.prefWidth = 70
  private val addExpenseEntryButton = Button("Add Expense")
  addExpenseEntryButton.disable <== addExpenseEntryTextField.text.isEmpty
  addExpenseEntryButton.onAction = { ev =>
    val description = addExpenseEntryTextField.text.get()
    addExpenseEntryTextField.text.value = ""
    val amount = addExpenseAmountTextField.text.get()
    addExpenseAmountTextField.text.value = "0.00 €"
    viewModel.addExpenseButtonPressed(description, amount)
  }

  add(viewModel.titleTextField, 0, 0, 2, 1)
  viewModel.titleTextField.setAlignment(Pos.CENTER)
  add(shareButton, 2, 0)

  add(bucketListView, 0, 1, 3, 1)

  add(addBucketListEntryTextField, 0, 2, 2, 1)
  add(addBucketListEntryButton, 2, 2)
  add(expenseView, 0, 3, 3, 1)
  add(addExpenseEntryTextField, 0, 4)
  add(addExpenseAmountTextField, 1, 4)
  add(addExpenseEntryButton, 2, 4)

  Platform.runLater(shareButton.requestFocus())
}
