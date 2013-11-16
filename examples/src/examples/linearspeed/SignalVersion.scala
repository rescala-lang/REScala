package examples.linearspeed

import react.events._
import react._
import macro.SignalMacro.{SignalM => Signal}



object SignalVersion extends App {

  val SPEED = 10
  val time = Var(0)  
  val space = Signal{ SPEED * time() }
  
  space.changed += ((x: Int) => println(x))
  
  // Equivalent to:
  //val e: Event[Int] = space.changed
  //val handler:  (Int => Unit) =  ((x: Int) => println(x))
  //e += handler
  
  while (true) {
    Thread sleep 20
    time() = time.getVal + 1
  }
  

}





















