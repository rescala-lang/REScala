package rescala

import rescala.log._

object Starter extends App {

  println("start!")

  // Enable default logging (with REScalaLogging project referenced)
  //rescala.ReactiveEngine.log = new MyLogging with AllLoggingEnabled

  val v1 = Var(1)
  val v2 = Var(2)
  val v3 = Var(3)

  val s = SignalSynt[Int](v1, v2, v3) { s =>
    println("evaluated")
    v1(s) +
      v2(s) +
      v3(s) + 1
  }

  v2() = 10

  println(s.get)

}
