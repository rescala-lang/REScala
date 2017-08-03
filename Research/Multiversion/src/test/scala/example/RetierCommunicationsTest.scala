package example

import rescala._
import retier.communicator.tcp._
import retier.registry.{Binding, Registry}
import retier.serializer.upickle._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import rescala.fullmv.transmitter.SignalTransmittable._

object Bindings {
  val testBinding = Binding[Int => Int]("test")
  val variableBinding = Binding[Signal[Int]]("variable")
}

object Main1 extends App {
  def test(x: Int) = { Thread.sleep(10) ; 2 * x }
  val variable = Var(0)

  val registry = new Registry
  registry.listen(TCP(1099))

  registry.bind(Bindings.testBinding)(test)
  registry.bind(Bindings.variableBinding)(variable)


  while (System.in.available() == 0) {
    variable transform { _ + 1 }
    Thread.sleep(1000)
  }
  registry.terminate()
}

object Main2 extends App {
  val registry = new Registry
  val remote = Await result (registry.request(TCP("localhost", 1099)), Duration.Inf)

  import scala.concurrent.ExecutionContext.Implicits._
//  val test: Int => Future[Int] = registry.lookup[Int => Int]("test", remote)
  val test: Int => Future[Int] = registry.lookup(Bindings.testBinding, remote)
  test(21).onComplete( x => println("test(21)"+ x))
  test(22).onComplete( x => println("test(22)"+ x))
  test(23).onComplete( x => println("test(23)"+ x))

//  val signal: Signal[Int] = Await result (registry.lookup[Signal[Int]]("variable", remote), Duration.Inf)
  val signal: Signal[Int] = Await result (registry.lookup(Bindings.variableBinding, remote), Duration.Inf)
  signal observe println

  while (System.in.available() == 0) {
    Thread.sleep(10)
  }
  registry.terminate()
}
