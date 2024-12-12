package ex2024travel.lofi_acl.example.monotonic_acl

import scalafx.application.Platform
import scalafx.geometry.Pos
import scalafx.scene.control.{Button, ListView, TextField, TextFormatter}
import scalafx.scene.layout.*
import scalafx.scene.text.Text

class TravelPlanView(viewModel: TravelPlanViewModel) extends GridPane {
  delegate.setMaxSize(Region.USE_COMPUTED_SIZE, Region.USE_COMPUTED_SIZE)

  private val shareButton = Button()
  shareButton.text = "Share"
  shareButton.onAction = ev => viewModel.createInviteButtonPressed()

  private val bucketListView: ListView[String] = ListView[String]()
  bucketListView.cellFactory = viewModel.bucketListCellFactory
  bucketListView.items = viewModel.bucketListIds
  bucketListView.prefHeight = 200
  bucketListView.maxHeight = Double.PositiveInfinity
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
  expenseView.maxHeight = Double.PositiveInfinity
  private val addExpenseEntryTextField = TextField()
  addExpenseEntryTextField.promptText = "Expense"
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

  {
    var rowIdx = 0
    add(viewModel.titleTextField, 0, rowIdx, 2, 1)
    viewModel.titleTextField.setStyle("-fx-font-size: 20px; -fx-font-weight: bold;")
    viewModel.titleTextField.alignment = Pos.Center
    shareButton.prefWidth = 100
    shareButton.vgrow = Priority.Always
    shareButton.maxHeight = 50
    add(shareButton, 2, rowIdx)
    rowIdx += 1
    val bucketListTitle = new HBox {
      children = new Text {
        text = "Bucket List"
        style = "-fx-font-size: 15px; -fx-font-weight: bold;"
      }
      alignment = scalafx.geometry.Pos.Center
    }
    add(bucketListTitle, 0, rowIdx, 3, 1)
    // --------------
    rowIdx += 1
    add(bucketListView, 0, rowIdx, 3, 1)
    // --------------
    rowIdx += 1
    add(addBucketListEntryTextField, 0, rowIdx, 2, 1)
    addBucketListEntryButton.prefWidth = 100
    add(addBucketListEntryButton, 2, rowIdx)
    // --------------
    rowIdx += 1
    val expenseListTitle = new HBox {
      children = new Text {
        text = "Expenses"
        style = "-fx-font-size: 15px; -fx-font-weight: bold;"
      }
      alignment = scalafx.geometry.Pos.Center
    }
    add(expenseListTitle, 0, rowIdx, 3, 1)
    // --------------
    rowIdx += 1
    add(expenseView, 0, rowIdx, 3, 1)
    // --------------
    rowIdx += 1
    add(addExpenseEntryTextField, 0, rowIdx)
    add(addExpenseAmountTextField, 1, rowIdx)
    addExpenseEntryButton.prefWidth = 100
    add(addExpenseEntryButton, 2, rowIdx)
  }

  Platform.runLater(shareButton.requestFocus())
}
