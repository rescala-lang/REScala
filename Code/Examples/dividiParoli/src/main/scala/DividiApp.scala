import java.io.IOException

import com.typesafe.scalalogging.Logger
import io.circe.generic.auto._
import javafx.fxml.FXMLLoader
import loci.communicator.ws.akka.WS
import loci.registry.{Binding, Registry}
import loci.serializer.circe._
import loci.transmitter.{IdenticallyTransmittable, RemoteRef}
import rescala.default._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property._
import scalafx.scene.Scene
import scalafx.scene.control._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.math.BigDecimal.RoundingMode

/** An example of  a BorderPane layout, with placement of children in the top,
  * left, center, right, and bottom positions.
  *
  * @see scalafx.scene.layout.BorderPane
  */
object DividiApp extends JFXApp {
  // display splashscreen and ask for username
  val enterNameDialog = new TextInputDialog(defaultValue = "Alice") {
    initOwner(stage)
    title = "Dividi"
    headerText = "Welcome to Dividi!"
    contentText = "Please enter your name:"
  }
  val username = enterNameDialog.showAndWait().getOrElse("")
  if (username == "") System.exit(0)

  // load new gui
  val resource = getClass.getResource("Gui.fxml")
  if (resource == null) {
    throw new IOException("Cannot load resource: Gui.fxml")
  }

  val onlineGui = BooleanProperty(true)
  val delayGui  = IntegerProperty(0)

  // define event fired on submit
  type Title     = String
  type Amount    = BigDecimal
  type Payer     = String
  type Timestamp = Long
  val logger: Logger = Logger("Dividi")

  case class Transaction(title: Title, amount: Amount, payer: Payer, sharedBetween: Set[Payer], timestamp: Timestamp) {
    override def toString: String = {
      val sharers = sharedBetween.toList.sorted
      if (sharers.length > 1)
        s"$payer paid $amount for $title. Shared between ${sharers.dropRight(1).mkString(", ")} and ${sharers.last}."
      else s"$payer paid $amount for $title. Shared between ${sharers.mkString(",")}."
    }
  }

  implicit val _transmittableGrowOnlyLog: IdenticallyTransmittable[PGrowOnlyLog[Transaction]] =
    IdenticallyTransmittable()

  // instanciate shared log
  @scala.annotation.nowarn
  val logBinding = Binding[PGrowOnlyLog[Transaction]]("log")
  val (registry, transactionLogDist): (Registry, PGrowOnlyLog[Transaction]) = {
    val registry = new Registry
    if (username == "Alice") { //server mode
      registry.listen(WS(1099))

      val newLog = PGrowOnlyLog[Transaction]()
      registry.bind(logBinding)(newLog)

      (registry, newLog)
    } else { // client mode
      val connection: Future[RemoteRef]                    = registry.connect(WS("ws://localhost:1099/"))
      val remote: RemoteRef                                = Await.result(connection, Duration.Inf)
      val subscribedLog: Future[PGrowOnlyLog[Transaction]] = registry.lookup(logBinding, remote)
      val log: PGrowOnlyLog[Transaction]                   = Await.result(subscribedLog, Duration.Inf)
      (registry, log)
    }
  }
  val transactionLog = transactionLogDist.crdtSignal

  // listen for new transactions and append them to the log
  val newTransaction: Evt[Transaction] = Evt[Transaction]()
  transactionLogDist.observe(newTransaction)

  // extract all people involved
  val peopleInvolved: Signal[Set[Payer]] = Signal {
    transactionLog().iterator.foldLeft(Set[Payer](username))((people, transaction) =>
      people + transaction.payer ++ transaction.sharedBetween
    )
  }

  // calculate a map keeping track of the debts of all users
  val debts: Signal[Map[Payer, Amount]] = Signal {
    transactionLog().iterator.foldLeft(Map[Payer, Amount]().withDefaultValue(0: Amount))((debts, transaction) => {
      val payer  = transaction.payer
      val amount = transaction.amount

      val share = {
        if (transaction.sharedBetween.nonEmpty)
          transaction.amount / transaction.sharedBetween.size
        else
          0: Amount
      }

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

  def resolveDebts(
      debts: Map[Payer, Amount],
      neededTransactions: List[(Payer, Payer, Amount)] = List()
  ): List[(Payer, Payer, Amount)] = {
    if (!debts.exists(_.getValue < 0))
      neededTransactions
    else {
      println(debts)
      println(neededTransactions)
      val maxDebtor = debts.minBy(debt => debt.getValue)._1 // find person with maximum debt
      println(s"Max debtor is $maxDebtor")
      // find best person to give money to
      val lenders  = debts.filter(_.getValue > 0).keys                      // find users without debt (lenders)
      val firstTry = (lenders.head, debts(lenders.head) + debts(maxDebtor)) // try first lender

      val bestChoice = lenders.foldLeft(firstTry: (Payer, Amount))(
        (currentBest, lender) => { // check if other lenders prove better (have payed amount closer to maxDebtor's debt)
          val thisTry = (lender, debts(lender) + debts(maxDebtor))
          if (thisTry._2.abs < currentBest._2.abs)
            thisTry
          else
            currentBest
        }
      )

      val lender = bestChoice._1
      val proposedTransaction = {
        if (bestChoice._2 > 0) // lend > debt
          (maxDebtor, lender, debts(maxDebtor).abs)
        else // debt > lend
          (maxDebtor, lender, debts(lender))
      }

      resolveDebts(
        debts + (maxDebtor -> (debts(maxDebtor) + proposedTransaction._3)) + (lender -> (debts(
          lender
        ) - proposedTransaction._3)),
        neededTransactions :+ proposedTransaction
      )
    }
  }

  // render FXML
  val root = FXMLLoader.load[Scene](resource)
  stage = new PrimaryStage() {
    title = s"Dividi: $username"
    scene = new Scene(root)
  }

  stage.onCloseRequest = (_: Any) => registry.terminate() // terminate registry on window close
}
