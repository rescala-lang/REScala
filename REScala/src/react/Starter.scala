package react

import react.log._
import react.log.StatisticsLogger
import react.log.ReactPlayerLog
import react.log.DotGraphLogger



object Starter extends App {

  println("start!")
  
  // Adding some loggers
  ReactiveEngine.log addLogger new ReactPlayerLog(
      new java.io.PrintStream(
      new java.io.FileOutputStream("./logs/Starter.txt", false))) 
  ReactiveEngine.log addLogger new DotGraphLogger(
      new java.io.PrintStream(
      new java.io.FileOutputStream("./logs/starter.dot", false))) 
  ReactiveEngine.log addLogger new StatisticsLogger(System.out)
  
  val v1 = Var(1)
  val v2 = Var(2)
  val v3 = Var(3)
  
  val s = StaticSignal(List(v1,v2,v3)){  println("evaluated");
    v1.getValue + 
    v2.getValue + 
    v3.getValue + 1
  }
  
  v2() = 10

  println(s.getVal)
  
}






