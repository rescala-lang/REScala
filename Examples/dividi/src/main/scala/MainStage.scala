import rescala._

import scala.math.BigDecimal.RoundingMode
import scalafx.Includes.when
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.binding.Bindings
import scalafx.geometry.Insets
import scalafx.geometry.Pos.{CenterLeft, TopRight}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text.Text

object MainStage extends JFXApp.PrimaryStage {
  title = s"Dividi: ${DividiApp.username}"
  width = 500
  height = 600
  resizable = true
  scene = new Scene {
    root = {
      val stdToolBar = new ToolBar {
        id = "standard"

        val onlineButton = new ToggleButton {
          minWidth = 100
          selected <==> DividiApp.onlineGui

          text <== when(selected) choose "Online" otherwise "Offline"
        }

        val delayLabel = new Label {
          text <== Bindings.createStringBinding(
            () =>
              Option(DividiApp.delayGui.value).getOrElse(0).toString + "s"
            , DividiApp.delayGui
          )
          alignment = TopRight
        }

        content = List(
          onlineButton,
          new HBox {
            prefWidth = 135
            alignment = CenterLeft
            children = List(new Label("Message Delay: "),
              delayLabel)
          },
          new Slider(0, 20, 0) {
            prefWidth = 150
            value <==> DividiApp.delayGui
            majorTickUnit = 1.0
            minorTickCount = 0
            //blockIncrement = 1.0
            //showTickMarks = true
            snapToTicks = true
          })
      }

      val debtOverviewBox = new HBox {
        padding = Insets(20)

        val debtOverview = new Text {
          val initial = "You are all set!"
          text() = initial
          DividiApp.howToSettle.changed += (newTransactions => {
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
              this.text() = initial
            else
              this.text() = settleTransactions
          })
        }
        debtOverview.setStyle("-fx-font-weight: bold")
        children = debtOverview
      }

      val logOutput = new TextArea {
        editable = false
        text = ""
        vgrow = Priority.Always
        //textAlignment = TextAlignment.Justify

        // bind output to log changes
        DividiApp.transactionLog.changed += (l => this.text() = l.mkString("\n"))
      }

      //peopleSelectionBox.setStyle("-fx-padding: 0 0 0 20;")

      val transactionPane = new TitledPane {
        text = "New Transaction"
        animated = true
        expanded = true

        val purpose = new TextField {
          promptText = "Enter Title..."
          DividiApp.newTransaction.observe(_ => this.text = "")
        }
        val amount = new TextField {
          promptText = "Enter Amount..."
          DividiApp.newTransaction.observe(_ => this.text = "")
        }

        val peopleSelectionBox = new VBox {
          spacing = 10
          val checkboxes: Signal[Seq[CheckBox]] = Signal {
            val l = DividiApp.peopleInvolved().toList.sorted
            DividiApp.logger.debug("updating checkboxes")
            l.map(person => new CheckBox {
              text = person.toString
              selected = false
              DividiApp.newTransaction.observe(_ => this.selected = false)
            })
          }
          checkboxes.changed += (boxes => Platform.runLater {
            this.children = boxes
          })
          children = checkboxes.now
        }

        val peopleInput = new TextField {
          promptText = "new Person"
          maxWidth = 135
          DividiApp.newTransaction.observe(_ => this.text = "")
        }

        val submitButton = new Button {
          text = "Submit"
          defaultButton = true
          maxWidth = 100
          maxHeight = 100
        }

        // add submit action
        submitButton.onAction = _ => {
          // transactionPane.expanded = false
          var peopleInvolved = peopleSelectionBox.checkboxes.now.filter(_.selected() == true).map(_.text()).toSet
          if (peopleInput.text() != "") peopleInvolved += peopleInput.text()
          val purposeText = purpose.text()
          val amountDecimal = BigDecimal(amount.text().toString).setScale(2, RoundingMode.CEILING)
          val payer = DividiApp.username
          val timestamp = System.currentTimeMillis
          // Transaction(title: Title, amount: Amount, payer: Payer, sharedBetween: Set[Payer], timestamp: Timestamp)
          DividiApp.newTransaction(DividiApp.Transaction(purposeText, amountDecimal, payer, peopleInvolved, timestamp))
        }

        content = new VBox {
          padding = Insets(20)
          spacing = 10
          children = List(
            new HBox(spacing = 10, purpose, amount),
            new Label("People involved:"),
            peopleSelectionBox,
            peopleInput,
            submitButton)
        }
      }

      val accordion = new Accordion {
        maxHeight = 150
        expandedPane = transactionPane
        panes = List(transactionPane)
      }

      new BorderPane {
        //maxWidth = 400
        //maxHeight = 300
        padding = Insets(0)
        // top = topRectangle
        center = new VBox(debtOverviewBox, logOutput)
        bottom = accordion
        top = stdToolBar
      }
    }
  }

}