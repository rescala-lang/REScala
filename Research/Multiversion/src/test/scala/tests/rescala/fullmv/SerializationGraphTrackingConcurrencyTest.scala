package tests.rescala.fullmv

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.collection.mutable.SortedSet
import scala.util.Random
import java.io.PrintStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.function.Consumer
import java.io.File
import rescala.fullmv.Transaction
import rescala.fullmv.SelfFirst
import java.awt.Desktop

class SerializationGraphTrackingConcurrencyTest extends FlatSpec with Matchers {
  "Serialization Graph Tracking" should "keep SSG acyclic under concurrent bombardment" in {
    val (_, transactions) = SerializationGraphTrackingConcurrencyTest.randomRun()

    var visitedAndStillOnStack = Map[Transaction, Boolean]()
    def searchForCycle(transaction: Transaction): Boolean = {
      visitedAndStillOnStack.get(transaction) match {
        case None =>
          visitedAndStillOnStack += transaction -> true
          transaction.successors.foreach(searchForCycle(_))
          visitedAndStillOnStack += transaction -> false
          false
        case Some(stillOnStack) =>
          stillOnStack
      }
    }
    transactions.find { searchForCycle(_) } should be(None)
  }
}

object SerializationGraphTrackingConcurrencyTest {
  type Node = SortedSet[Transaction]

  def main(args: Array[String]): Unit = {
    val (_, transactions) = randomRun()
    SerializationGraphTrackingConcurrencyTest.postProcess(transactions)
  }

  // do not use this in practice; x.ensureAndGetOrder(y) is order-sensitive for fairness, but compare(x, y) is not.
  val ordering = new Ordering[Transaction] {
    override def compare(x: Transaction, y: Transaction): Int = {
      if (x == y) 0 else if (x.ensureAndGetOrder(y) == SelfFirst) -1 else 1
    }
  }

  def randomRun(): (Iterable[Node], Iterable[Transaction]) = {
    // test configuration
    val cores = Runtime.getRuntime().availableProcessors()
    val numNodes = 128 * cores
    val numTransactionsPerThread = 32
    val numOpsPerTransaction = 16

    // instantiate everything
    val nodes = (0 until numNodes) map { _ => newNode() }
    class Runner(val index: Int) extends Runnable {
      val queue = Seq[Transaction]((0 until numTransactionsPerThread).map { i => Transaction(s"T($index,$i)") }: _*)
      override def run(): Unit = {
        for (transaction <- queue; op <- 0 until numOpsPerTransaction) {
          val nodeId = Random.nextInt(nodes.size)
          val node = nodes(nodeId)
          // println(f"Runner-$index%d ${transaction.data}%s Op-$op%02d: (a) entering monitor for node ${node._1}%02d")
          node.synchronized {
            // println(f"Runner-$index%d ${transaction.data}%s Op-$op%02d: (b) Inserting on node ${node._1}%02d -> ${node._2.map(_.data)}%s")
            node.add(transaction)
            // println(f"Runner-$index%d ${transaction.data}%s Op-$op%02d: (c) done")
          }
          // println(f"Runner-$index%d ${transaction.data}%s Op-$op%02d: (d) exited monitor")
        }
      }
    }
    val runners = (0 until cores) map { new Runner(_) }
    val transactions = runners.flatMap { _.queue }

    // execute all runners
    val threads = runners.tail.map(runner => new Thread(runner, s"Runner-${runner.index}"))
    for (thread <- threads) thread.start()
    runners.head.run()
    for (thread <- threads) thread.join()

    (nodes, transactions)
  }

  def newNode(): Node = SortedSet[Transaction]()(ordering)

  def postProcess(transactions: Iterable[Transaction]): Unit = {
    val pdf = File.createTempFile("ssg-dot-viz", ".pdf")
    println(s"[POST] Starting Postprocessing!")
    println(s"[POST] Transitive reduction...")
    val edges = edgesFromTransactions(transactions)
    val totalEdges = countEdges(edges)
    val reduced = transitiveReduction(edges)
    val reducedEdges = countEdges(reduced)
    val difference = totalEdges - reducedEdges
    val percentage = difference.toFloat / totalEdges * 100
    println(f"[POST] Removed $difference%d of $totalEdges%d edges ($percentage%.2f%%).")
    println(s"[POST] Sending reduced SSG to dot...")
    val dot = Runtime.getRuntime.exec(Array[String]("dot", "-Tpdf", "-o" + pdf.getAbsolutePath()))

    new Thread(new Runnable() {
      override def run(): Unit = {
        new BufferedReader(new InputStreamReader(dot.getInputStream())).lines().forEach(new Consumer[String]() {
          override def accept(line: String): Unit = println("[DOT] " + line)
        })
      }
    }, "DOT-stdout").start()
    new Thread(new Runnable() {
      override def run(): Unit = {
        new BufferedReader(new InputStreamReader(dot.getErrorStream())).lines().forEach(new Consumer[String]() {
          override def accept(line: String): Unit = System.err.println("[DOT] " + line)
        })
      }
    }, "DOT-stderr").start()
    printDigraphDot(reduced, new PrintStream(dot.getOutputStream))
    dot.getOutputStream.close()
    println(s"[POST] dot rendering...")
    val dotExitCode = dot.waitFor()
    if (dotExitCode != 0) {
      System.err.println(s"[POST] Rendering returned non-zero exit code $dotExitCode, skipping pdf viewer")
    } else {
      println(s"[POST] Rendering completed.")
      Desktop.getDesktop().open(pdf)
    }
    println(s"[POST] Postprocessing completed!")
  }
  def edgesFromTransactions(transactions: Iterable[Transaction]): Map[Transaction, Set[Transaction]] = {
    transactions.map { transaction =>
      transaction -> transaction.successors
    }.toMap
  }
  def countEdges(edges: Map[Transaction, Set[Transaction]]): Int = edges.map(_._2.size).sum
  def transitiveReduction(edges: Map[Transaction, Set[Transaction]]): Map[Transaction, Set[Transaction]] = {
    edges.foldLeft(edges) {
      case (reduced, (transaction, outgoing)) =>
        outgoing.foldLeft(reduced) {
          case (reduced, successor) =>
            reduced(successor).foldLeft(reduced) {
              case (reduced, transitive) =>
                reduced + (transaction -> (reduced(transaction) - transitive))
            }
        }
    }
  }
  def printDigraphDot(edges: Map[Transaction, Set[Transaction]], out: PrintStream = System.out): Unit = {
    out.println("digraph SSG {")
    for (
      (transaction, outgoing) <- edges;
      successor <- outgoing
    ) {
      out.println("\t\"" + transaction.data + "\" -> \"" + { successor.data } + "\"")
    }
    out.println("}")
  }
}
