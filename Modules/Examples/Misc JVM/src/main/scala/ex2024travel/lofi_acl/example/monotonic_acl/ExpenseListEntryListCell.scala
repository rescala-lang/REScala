package ex2024travel.lofi_acl.example.monotonic_acl

import javafx.scene.control.ListCell
import javafx.scene.layout.HBox
import ex2024travel.lofi_acl.example.monotonic_acl.ExpenseListEntryListCell.amountTextFilter
import scalafx.beans.property.StringProperty
import scalafx.event.subscriptions.Subscription
import scalafx.scene.control.{TextField, TextFormatter}
import scalafx.scene.layout.Priority

import scala.collection.mutable

class ExpenseListEntryListCell(model: TravelPlanModel) extends ListCell[String] {
  private val subscriptions = mutable.Buffer.empty[Subscription]

  override def updateItem(expenseId: String, empty: Boolean): Unit = {
    super.updateItem(expenseId, empty)

    subscriptions.foreach(_.cancel())
    subscriptions.clear()

    if empty || expenseId == null then {
      setGraphic(null)
      return
    }

    val (descriptionInModel, amountInModel, commentInModel) = model.expenseListProperties.get()(expenseId)

    val descriptionTextField = TextField()
    descriptionTextField.text = descriptionInModel.get()
    descriptionTextField.prefWidth = 130
    val amountTextField = TextField()
    amountTextField.text = amountInModel.get()
    amountTextField.textFormatter = TextFormatter(amountTextFilter)
    amountTextField.prefWidth = 66
    val commentTextField = TextField()
    commentTextField.text = commentInModel.get()
    commentTextField.promptText = "Comment"
    commentTextField.prefWidth = 130
    commentTextField.hgrow = Priority.Always

    bindBidirectionalWithFocusFilter(
      descriptionTextField,
      descriptionInModel,
      newText => model.setExpenseDescription(expenseId, newText)
    )

    bindBidirectionalWithFocusFilter(
      amountTextField,
      amountInModel,
      newAmount => model.setExpenseAmount(expenseId, newAmount)
    )

    bindBidirectionalWithFocusFilter(
      commentTextField,
      commentInModel,
      newText => model.setExpenseComment(expenseId, newText)
    )

    setGraphic(HBox(descriptionTextField, amountTextField, commentTextField))
  }

  private def bindBidirectionalWithFocusFilter(
      textField: TextField,
      modelProperty: StringProperty,
      update: String => Unit
  ): Unit = {
    subscriptions += modelProperty.onChange((_, _, newText) =>
      if !textField.isFocused then
        textField.text = newText
    )
    subscriptions += textField.text.onChange((_, _, newText) =>
      if textField.isFocused then
        update(newText)
    )
    subscriptions += textField.focused.onChange((_, wasFocused, isNowFocused) =>
      if !isNowFocused then
        textField.text = modelProperty.get()
    )
  }
}

object ExpenseListEntryListCell {
  private val pattern = "\\d+(.\\d\\d)? €".r

  val amountTextFilter: TextFormatter.Change => TextFormatter.Change = (change: TextFormatter.Change) =>
    if pattern.matches(change.controlNewText) then change
    else null
}
