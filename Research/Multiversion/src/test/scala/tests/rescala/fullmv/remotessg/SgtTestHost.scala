package tests.rescala.fullmv.remotessg

import java.rmi.registry.LocateRegistry
import java.rmi.server.UnicastRemoteObject
import java.rmi.{Naming, Remote, RemoteException}

import rescala.fullmv.{Host, Transaction}
import tests.rescala.fullmv.SerializationGraphTrackingTest.Node
import tests.rescala.fullmv.{SerializationGraphTrackingTest, TestRemoteHost}

import scala.util.Random

trait SgtTestWorker extends Remote {
  @throws[RemoteException]
  def runTransactions(parallel: Int, sequential: Int, ops: Int, workers: IndexedSeq[SgtTestWorker]): Iterable[Transaction]
  @throws[RemoteException]
  def accessNode(idx: Int, txn: Transaction): Unit
  @throws[RemoteException]
  def getNodes(): IndexedSeq[Node]
  @throws[RemoteException]
  def done(): Unit
}

trait SgtTestWorkerLogin extends Remote {
  @throws[RemoteException]
  def loginWorker(worker: SgtTestWorker): Int
}

class SgtTestRandomWorker extends UnicastRemoteObject with SgtTestWorker {
  val nodes = (1 to 64) map (_ => SerializationGraphTrackingTest.newNode())
  var id = -1

  override def runTransactions(parallel: Int, sequential: Int, ops: Int, workers: IndexedSeq[SgtTestWorker]): Iterable[Transaction] = {
    var transactions = Set[Transaction]()
    val threads = for (par <- 0 until parallel) yield {
      new Thread(new Runnable {
        override def run() = {
          for (seq <- 0 until sequential) {
            val transaction = Transaction(s"T($id-$par-$seq)")
            println("starting transaction " + transaction)
            for (op <- 0 until ops) {
              val hostIdx = Random.nextInt(workers.size)
              val nodeIdx = Random.nextInt(64)
              if(hostIdx == id) {
                println(transaction + " accessing LocalNode(" + id + "-" + nodeIdx + ")")
                accessNode0(nodeIdx, transaction)
              } else {
                println(transaction + " accessing RemoteNode(" + id + "-" + nodeIdx + ")")
                workers(hostIdx).accessNode(nodeIdx, transaction)
              }
            }
            SgtTestRandomWorker.this.synchronized {
              transactions += transaction
            }
          }
        }
      })
    }
    threads.foreach(_.start())
    threads.foreach(_.join())
    transactions
  }

  override def accessNode(idx: Int, txn: Transaction): Unit = {
    println(txn + " accessing LocalNode(" + id + "-" + idx + ")")
    accessNode0(idx, txn)
  }

  private def accessNode0(idx: Int, txn: Transaction) = {
    nodes(idx).synchronized(nodes(idx).add(txn))
  }

  override def getNodes(): IndexedSeq[Node] = nodes

  var retrieved = false

  override def done() = synchronized {
    retrieved = true
    notifyAll()
  }

  def await() = synchronized {
    while(!retrieved) {
      wait()
    }
  }
}

object SgtTestRandomWorker {
  def main(args: Array[String]): Unit = {
    val login = Naming.lookup("rmi://localhost:" + TestRemoteHost.PORT + "/" + "login").asInstanceOf[SgtTestWorkerLogin]
    val worker = new SgtTestRandomWorker
    worker.id = login.loginWorker(worker)
    println("Registered as worker " + worker.id)
    worker.await()
    Host.shutdown(false)
    UnicastRemoteObject.unexportObject(worker, false)
  }
}

object SgtTest {
  def main(args: Array[String]): Unit = {
    var workers = IndexedSeq[SgtTestWorker]()
    object login extends UnicastRemoteObject with SgtTestWorkerLogin {
      @throws[RemoteException]
      override def loginWorker(worker: SgtTestWorker): Int = {
        SgtTest.synchronized {
          workers :+= worker
          println(workers.size + " workers connected.")
          workers.size - 1
        }
      }
    }
    val registry = LocateRegistry.createRegistry(TestRemoteHost.PORT)
    registry.bind("login", login)
    println("Login up, now accepting workers. Press Enter to start run.")

    System.in.read()

    registry.unbind("login")
    UnicastRemoteObject.unexportObject(login, false)
    println("Doing run with " + workers.size + " workers.")

    var transactions = Map[SgtTestWorker, Iterable[Transaction]]()
    val threads = workers.map{ worker =>
      new Thread(new Runnable {
        override def run() = {
          val workerTransactions = worker.runTransactions(2, 32, 8, workers)
          SgtTest.synchronized{
            transactions += worker -> workerTransactions
          }
        }
      })
    }

    threads.foreach(_.start())
    threads.foreach(_.join())
    val nodes = workers.map(worker => worker -> worker.getNodes()).toMap
    workers.foreach(_.done())
    Host.shutdown(false)

    println("Collected Transactions:")
    println("\t"+transactions.mapValues(_.mkString("\n\t\t")).mkString("\n\t"))
    println("Collected Nodes:")
    println("\t"+nodes.mapValues(_.mkString("\n\t\t")).mkString("\n\t"))

    SerializationGraphTrackingTest.postProcess(transactions.values.flatten, Some((8, 2)))
  }
}
