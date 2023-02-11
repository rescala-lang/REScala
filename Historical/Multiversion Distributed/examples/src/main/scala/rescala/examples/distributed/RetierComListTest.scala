//package rescala.examples.distributed
//
//import rescala.fullmv.FullMVEngine
//import rescala.fullmv.FullMVEngine.default._
//import loci.communicator.tcp._
//import loci.registry.{Binding, Registry}
//
//import scala.concurrent.duration._
//import scala.concurrent.{Await, Future}
//
//object Bindings1 {
//  import rescala.fullmv.transmitter.ReactiveTransmittable._
//  import io.circe.generic.auto._
//  import rescala.fullmv.transmitter.CirceSerialization._
//  implicit val host: FullMVEngine = explicitEngine
//
//  val eventBinding = Binding[Event[Int]]("listEvt")
//  val variableBinding1 = Binding[Signal[List[Int]]]("variable")
//  val addBinding = Binding[Event[Int] => Unit]("listAdd")
//}
////This method represents the server
//object Server extends App {
//  val eventList :   Var[List[Event[Int]]]= Var(List())
//  def eventAdd(x: Event[Int]) = {
//    threadPool.submit(new Runnable {
//      // need to move off of the network receive thread to not block it
//      override def run(): Unit = {
//        try {
//          eventList() = eventList.now :+ x
//        } catch {
//          case e: Throwable => new Exception("Client Connect processing failed", e).printStackTrace()
//        }
//      }
//    })
//  }
//  val testList1 = eventList.flatten.fold(List(1,2,3)) { (list, adds) =>
//    list ++ adds.flatten
//  }
//  testList1 observe println
//
//  val registry = new Registry
//  registry.listen(TCP(1099))
//
//  registry.bind(Bindings1.variableBinding1)(testList1)
//  registry.bind(Bindings1.addBinding)(eventAdd)
//
//  while (System.in.available() == 0) {
//   Thread.sleep(10)
//  }
//  registry.terminate()
//}
//// This method represents a client. Multiple clients may be active at a time.
//object Client extends App {
//  val registry = new Registry
//  val remote = Await result (registry.request(TCP("localhost", 1099)), Duration.Inf)
//
//  val listOnServer: Signal[List[Int]] = Await result (registry.lookup(Bindings1.variableBinding1, remote), Duration.Inf)
//  listOnServer observe println
//
//  var input =""
//  var continueProgram = true
//
//  val eventAdd: Event[Int] =>  Future[Unit] =  registry.lookup(Bindings1.addBinding, remote)
//  val e1 = Evt[Int]()
//  eventAdd(e1)
//
//  while (continueProgram) {
//    println("enter \"add\" to add a vaule or \"end\" to end")
//    input = scala.io.StdIn.readLine()
//    if (input == "end"){
//      continueProgram = false
//    }else if (input == "add"){
//      println("enter a value")
//      input = scala.io.StdIn.readLine()
//      try
//      {
//        e1(input.toInt)
//      }
//      catch
//      {
//        case e:Exception => println("please enter a valid number")
//      }
//    }
//    Thread.sleep(10)
//  }
//
//
//  registry.terminate()
//}
