package react

import react.log._



object Starter extends App {

  println("start!")
  
  val reactplayerLogger = new ReactPlayerLog(
      new java.io.PrintStream(
      new java.io.FileOutputStream("./logs/Starter.txt", false)))
  
  val dotLogger = new DotGraphLogger(
      new java.io.PrintStream(
      new java.io.FileOutputStream("./logs/starter.dot", false)))
 
  ReactiveEngine.log addLogger reactplayerLogger
  ReactiveEngine.log addLogger dotLogger
  
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
  
  dotLogger.snapshot
  
  
}






