//package example
//
//import rescala.fullmv.FullMVEngine
//import rescala.fullmv.FullMVEngine.default._
//import retier.communicator.tcp._
//import retier.registry.{Binding, Registry}
//
//import scala.concurrent.duration._
//import scala.concurrent.{Await, Future}
//
//object Bindings {
//  import rescala.fullmv.transmitter.ReactiveTransmittable._
//  import io.circe.generic.auto._
//  import rescala.fullmv.transmitter.CirceSerialization._
//  implicit val host: FullMVEngine = explicitEngine
//
//  val testBinding = Binding[Int => Int]("test")
//  val variableBinding = Binding[Signal[Int]]("variable")
//}
//
//object Main1 extends App {
//  def test(x: Int) = { Thread.sleep(x) ; 2 * x }
//  val variable = Var(0)
//
//  val registry = new Registry
//  registry.listen(TCP(1099))
//
//  registry.bind(Bindings.testBinding)(test)
//  registry.bind(Bindings.variableBinding)(variable)
//
//
//  while (System.in.available() == 0) {
//    variable transform { _ + 1 }
//    Thread.sleep(1000)
//  }
//  registry.terminate()
//}
//
//object Main2 extends App {
//  val registry = new Registry
//  val remote = Await result (registry.request(TCP("localhost", 1099)), Duration.Inf)
//
//  import scala.concurrent.ExecutionContext.Implicits._
////  val test: Int => Future[Int] = registry.lookup[Int => Int]("test", remote)
//  val test: Int => Future[Int] = registry.lookup(Bindings.testBinding, remote)
//  test(1000).onComplete( x => println("test(1000)" + x))
//  test(50).onComplete( x => println("test(50)"+ x))
//
////  val signal: Signal[Int] = Await result (registry.lookup[Signal[Int]]("variable", remote), Duration.Inf)
//  val signal: Signal[Int] = Await result (registry.lookup(Bindings.variableBinding, remote), Duration.Inf)
//  signal observe println
//
//  while (System.in.available() == 0) {
//    Thread.sleep(10)
//  }
//  registry.terminate()
//}
