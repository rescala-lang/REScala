import akka.actor.{ActorRef, ActorSystem}
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.Logger
import rescala._
import rescala.crdts.pvars._

import scala.math.BigDecimal.RoundingMode
import scalafx.Includes.{when, _}
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.binding.Bindings
import scalafx.beans.property._
import scalafx.geometry.Insets
import scalafx.geometry.Pos.{CenterLeft, TopRight}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.scene.text.Text


/** An example of  a BorderPane layout, with placement of children in the top,
  * left, center, right, and bottom positions.
  *
  * @see scalafx.scene.layout.BorderPane
  */
object DividiApp extends JFXApp {
  // ask for username
  val enterNameDialog: TextInputDialog = new TextInputDialog(defaultValue = "Alice") {
    initOwner(stage)
    title = "Dividi"
    headerText = "Welcome to Dividi!"
    contentText = "Please enter your name:"
  }
  val username: String = enterNameDialog.showAndWait().getOrElse("")
  if (username == "") System.exit(0)

  val port: Int = {
    if (username == "Alice") 2500
    else if (username == "Bob") 2501
    else if (username == "Charlie") 2502
    else 2503
  }

  // create an Akka system & engine
  val config: Config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
    withFallback(ConfigFactory.load())
  val system = ActorSystem("ClusterSystem", config)

  implicit val engine: ActorRef = system.actorOf(DistributionEngine.props(username), username)

  val onlineGui = BooleanProperty(true)
  val delayGui = IntegerProperty(0)

  // bind gui properties to engine
  onlineGui.onChange((_, _, newVal) => {
    DistributionEngine.setOnline(newVal.booleanValue())
    if (newVal.booleanValue())
      logger.debug(s"Setting engine to online mode")
    else
      logger.debug(s"Setting engine to offline mode")
  })
  delayGui.onChange((_, _, newVal) => {
    DistributionEngine.setDelay(newVal.longValue() * 1000)
    logger.debug(s"Setting engine delay to ${newVal.longValue() * 1000}ms")
  })

  // define event fired on submit
  type Title = String
  type Amount = BigDecimal
  type Payer = String
  type Timestamp = Long
  val logger: Logger = Logger("Dividi")

  case class Transaction(title: Title, amount: Amount, payer: Payer, sharedBetween: Set[Payer], timestamp: Timestamp) {
    override def toString: String = {
      val sharers = sharedBetween.toList.sorted
      if (sharers.length > 1) s"$payer paid $amount for $title. Shared between ${sharers.dropRight(1).mkString(", ")} and ${sharers.last}."
      else s"$payer paid $amount for $title. Shared between ${sharers.mkString(",")}."
    }
  }

  // instanciate shared log
  val transactionLog: PGrowOnlyLog[Transaction] = PGrowOnlyLog[Transaction]()
  transactionLog.publish("TransactionLog")

  val newTransaction: rescala.Evt[Transaction] = Evt[Transaction]()
  // listen for new transactions and append them to the log
  //newTransaction.observe(transaction => transactionLog.append(transaction))
  transactionLog.observe(newTransaction)

  // extract all people involved
  val peopleInvolved: Signal[Set[Payer]] = Signal {
    transactionLog().foldLeft(Set[Payer](username))((people, transaction) =>
      people + transaction.payer ++ transaction.sharedBetween)
  }

  // calculate a map keeping track of the debts of all users
  val debts: Signal[Map[Payer, Amount]] = Signal {
    transactionLog().foldLeft(Map[Payer, Amount]().withDefaultValue(0: Amount))((debts, transaction) => {
      val payer = transaction.payer
      val amount = transaction.amount
      val share = transaction.amount / transaction.sharedBetween.size

      // map with updated debt for all people involved in transaction
      val updatedDebtorEntries = transaction.sharedBetween.foldLeft(debts)((map, debtor) => {
        map + (debtor -> (map(debtor) - share).setScale(2, RoundingMode.CEILING))
      })

      // add positive amount for payer
      updatedDebtorEntries + (payer -> (updatedDebtorEntries(payer) + amount))
    })
  }

  // propose transactions to settle debts
  val howToSettle: Signal[List[(Payer, Payer, Amount)]] = debts.map(resolveDebts(_))

  def resolveDebts(debts: Map[Payer, Amount], neededTransactions: List[(Payer, Payer, Amount)] = List()): List[(Payer, Payer, Amount)] = {
    if (!debts.exists(_.getValue < 0))
      neededTransactions
    else {
      println(debts)
      println(neededTransactions)
      val maxDebtor = debts.minBy(debt => debt.getValue)._1 // find person with maximum debt
      println(s"Max debtor is $maxDebtor")
      // find best person to give money to
      val lenders = debts.filter(_.getValue > 0).keys // find users without debt (lenders)
      val firstTry = (lenders.head, debts(lenders.head) + debts(maxDebtor)) // try first lender

      val bestChoice = lenders.foldLeft(firstTry: (Payer, Amount))((currentBest, lender) => { // check if other lenders prove better (have payed amount closer to maxDebtor's debt)
        val thisTry = (lender, debts(lender) + debts(maxDebtor))
        if (thisTry._2.abs < currentBest._2.abs)
          thisTry
        else
          currentBest
      })

      val lender = bestChoice._1
      val proposedTransaction = {
        if (bestChoice._2 > 0) // lend > debt
          (maxDebtor, lender, debts(maxDebtor).abs)
        else // debt > lend
          (maxDebtor, lender, debts(lender))
      }

      resolveDebts(debts + (maxDebtor -> (debts(maxDebtor) + proposedTransaction._3)) + (lender -> (debts(lender) - proposedTransaction._3)), neededTransactions :+ proposedTransaction)
    }
  }

  stage = new JFXApp.PrimaryStage {
    title = s"Dividi: $username"
    width = 500
    height = 600
    resizable = true
    scene = new Scene {
      root = {
        val stdToolBar = new ToolBar {
          id = "standard"

          val onlineButton: ToggleButton = new ToggleButton {
            minWidth = 100
            selected <==> onlineGui

            text <== when(selected) choose "Online" otherwise "Offline"
          }

          val delayLabel: Label = new Label {
            text <== Bindings.createStringBinding(
              () =>
                Option(delayGui.value).getOrElse(0).toString + "s"
              , delayGui
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
              value <==> delayGui
              majorTickUnit = 1.0
              minorTickCount = 0
              //blockIncrement = 1.0
              //showTickMarks = true
              snapToTicks = true
            })
        }

        val debtOverviewBox = new HBox {
          padding = Insets(20)

          val debtOverview: Text {
            val initial: Payer
          } = new Text {
            val initial = "You are all set!"
            text() = initial
            howToSettle.changed += (newTransactions => {
              val settleTransactions = newTransactions.map(transaction => {
                val sender = transaction._1
                val receiver = transaction._2
                val amount = transaction._3

                if (sender == username)
                  s"You owe $receiver $amount. Transfer it!"
                else if (receiver == username)
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
          transactionLog.changed += (l => this.text() = l.mkString("\n"))
        }

        //peopleSelectionBox.setStyle("-fx-padding: 0 0 0 20;")

        val transactionPane = new TitledPane {
          text = "New Transaction"
          animated = true
          expanded = true

          val purpose: TextField = new TextField {
            promptText = "Enter Title..."
            newTransaction.observe(_ => this.text = "")
          }
          val amount: TextField = new TextField {
            promptText = "Enter Amount..."
            newTransaction.observe(_ => this.text = "")
          }

          object peopleSelectionBox extends VBox {
            spacing = 10
            val checkboxes: Signal[Seq[CheckBox]] = Signal {
              val l = peopleInvolved().toList.sorted
              logger.debug("updating checkboxes")
              l.map(person => new CheckBox {
                text = person.toString
                selected = false
                newTransaction.observe(_ => this.selected = false)
              })
            }
            checkboxes.changed += (boxes => Platform.runLater {
              this.children = boxes
            })
            children = checkboxes.now
          }

          val peopleInput: TextField = new TextField {
            promptText = "new Person"
            maxWidth = 135
            newTransaction.observe(_ => this.text = "")
          }

          val submitButton: Button = new Button {
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
            val payer = username
            val timestamp = System.currentTimeMillis
            // Transaction(title: Title, amount: Amount, payer: Payer, sharedBetween: Set[Payer], timestamp: Timestamp)
            newTransaction.fire(Transaction(purposeText, amountDecimal, payer, peopleInvolved, timestamp))
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
  stage.onCloseRequest = _ => system.terminate()
}
