package example

import retier.communicator.tcp._
import retier.registry.{Binding, Registry}
import retier.serializer.upickle._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


object Bindings {
  val testBinding = Binding[Int => Int]("test")
//  val variableBinding = Binding[Signal[Int]]("variable")
}


object Main1 extends App {
  def test(x: Int) = 2 * x
//  val variable = Var(0)


  val registry = new Registry
  registry.listen(TCP(1099))

  registry.bind(Bindings.testBinding)(test)
//  registry.bind(Bindings.variableBinding)(variable)


  while (true) {
//    variable transform { _ + 1 }
    Thread.sleep(1000)
  }
}


object Main2 extends App {
  val registry = new Registry
  val remote = Await result (registry.request(TCP("localhost", 1099)), Duration.Inf)


//  val test: Int => Future[Int] = registry.lookup[Int => Int]("test", remote)
  val test: Int => Future[Int] = registry.lookup(Bindings.testBinding, remote)
  println(Await result (test(21), Duration.Inf))

//  val signal: Signal[Int] = Await result (registry.lookup[Signal[Int]]("variable", remote), Duration.Inf)
//  val signal: Signal[Int] = Await result (registry.lookup(Bindings.variableBinding, remote), Duration.Inf)
//  signal observe println
}
