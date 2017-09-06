import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import rescala.crdts.pvars._
import rescala._

import scala.math.BigDecimal.RoundingMode
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property._
import scalafx.scene.control._


/** An example of  a BorderPane layout, with placement of children in the top,
  * left, center, right, and bottom positions.
  *
  * @see scalafx.scene.layout.BorderPane
  */
object DividiApp extends JFXApp {
  // ask for username
  val enterNameDialog = new TextInputDialog(defaultValue = "Alice") {
    initOwner(stage)
    title = "Dividi"
    headerText = "Welcome to Dividi!"
    contentText = "Please enter your name:"
  }
  val username = enterNameDialog.showAndWait().getOrElse("")
  if (username == "") System.exit(0)

  val port: Int = {
    if (username == "Alice") 2500
    else if (username == "Bob") 2501
    else if (username == "Charlie") 2502
    else 2503
  }

  // create an Akka system & engine
  val config = ConfigFactory.parseString("akka.remote.netty.tcp.port=" + port).
    withFallback(ConfigFactory.load())
  val system = ActorSystem("ClusterSystem", config)

  implicit val engine = system.actorOf(DistributionEngine.props(username), username)

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
  val transactionLog = PGrowOnlyLog[Transaction]()
  transactionLog.publish("TransactionLog")

  val newTransaction = Evt[Transaction]()
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

  stage = MainStage
  stage.onCloseRequest = _ => system.terminate()
}
