package examples.speed


import react._
import macro.SignalMacro.{SignalM => Signal}






class SignalVersion extends App {

  val SPEED = 10
  val time = Var(0)  
  val space = Signal{ SPEED * time() }
  
  space.changed += ((x: Int) => println(x)) 
  
  
  while (true) {
    Thread sleep 20
    time() = time.getVal + 1
  }

}





















