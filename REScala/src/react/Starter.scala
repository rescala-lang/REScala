package react

import react.log._



object Starter extends App {

  println("start!")
 
  ReactiveEngine.log addLogger
    new ReactPlayerLog(
      new java.io.PrintStream(
      new java.io.FileOutputStream("./logs/Starter.txt", false)))
  
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






