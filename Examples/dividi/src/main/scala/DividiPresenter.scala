import DividiApp.Payer
import com.jfoenix.controls._
import rescala.default._

import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode
import scalafx.beans.property.StringProperty
import scalafx.event.ActionEvent
import scalafx.scene.control.{CheckBox, ChoiceBox, TextArea, TextField}
import scalafx.scene.layout.{GridPane, VBox}
import scalafx.scene.text.{Text, TextFlow}
import scalafxml.core.macros.sfxml
import scalafx.Includes._
import scalafx.application.Platform

@sfxml(additionalControls = List("com.jfoenix.controls"))
class DividiPresenter(private val onlineButton: JFXToggleButton,
                      private val delaySlider: JFXSlider,
                      private val debtOutputFlow: TextFlow,
                      private val debtOutput: Text,
                      private val logOutput: Text,
                      private val titleField: JFXTextField,
                      private val amountField: JFXTextField,
                      private val peopleCheckboxes: VBox,
                      private val newPersonField: JFXTextField,
                      private val submitButton: JFXButton) {

  // initialize interface
  //showDebtOutput(false)
  //debtOutput.text.onChange((_, _, newVal) => if (newVal != "") showDebtOutput(false) else showDebtOutput(true))

  // bind toolbar
  onlineButton.selectedProperty() <==> DividiApp.onlineGui
  delaySlider.valueProperty() <==> DividiApp.delayGui

  // bind checkboxes
  val checkboxes: Signal[Seq[JFXCheckBox]] = Signal {
    val l = DividiApp.peopleInvolved().toList.sorted
    DividiApp.logger.debug("updating checkboxes")
    l.map(person => {
      val checkBox = new JFXCheckBox(person.toString)
      DividiApp.newTransaction.observe(_ => checkBox.selected = false)
      checkBox
    })
  }
  checkboxes.now.foreach(box => peopleCheckboxes.getChildren().add(box))
  checkboxes.changed += (boxes => Platform.runLater {{
    val children = peopleCheckboxes.getChildren()
    children.clear()
    boxes.foreach(box => children.add(box))
  }})

  // bind logOutput
  DividiApp.transactionLog.changed += (l => logOutput.text() = l.mkString("\n\n"))

  // bind debtOutput
  val initial = "You are all set!"
  DividiApp.howToSettle.changed += (newTransactions => Platform.runLater {
    {
      val settleTransactions = newTransactions.map(transaction => {
        val sender = transaction._1
        val receiver = transaction._2
        val amount = transaction._3

        if (sender == DividiApp.username)
          s"You owe $receiver $amount. Transfer it!"
        else if (receiver == DividiApp.username)
          s"$sender owes you $amount."
        else
          s"$sender owes $receiver $amount."
      }).mkString("\n")

      if (settleTransactions.isEmpty)
        debtOutput.text() = initial
      else
        debtOutput.text() = settleTransactions
    }
  })
  debtOutput.text() = initial

  def handleSubmit(event: ActionEvent) {
    val purpose = titleField.getText
    val amount = amountField.getText()
    val newPerson = newPersonField.getText()
    val amountDecimal = BigDecimal(amount).setScale(2, RoundingMode.CEILING)
    val payer = DividiApp.username
    val timestamp = System.currentTimeMillis

    var peopleInvolved: Set[Payer] = Set()

    peopleCheckboxes.children.filtered(n => n.isInstanceOf[JFXCheckBox]).forEach(n => {
      val checkBox = n.asInstanceOf[JFXCheckBox]
      if (checkBox.isSelected) peopleInvolved += checkBox.getText
    })

    if (newPerson != "") peopleInvolved += newPerson

    if (purpose != "" && amount != "") {
      println(s"$payer payed $amount for $purpose. People involved: $peopleInvolved")

      // Transaction(title: Title, amount: Amount, payer: Payer, sharedBetween: Set[Payer], timestamp: Timestamp)
      DividiApp.newTransaction(DividiApp.Transaction(purpose, amountDecimal, payer, peopleInvolved, timestamp))

      titleField.textProperty().setValue("")
      amountField.textProperty().setValue("")
      newPersonField.textProperty().setValue("")
    }
  }
}
