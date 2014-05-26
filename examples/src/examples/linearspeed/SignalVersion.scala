package examples.linearspeed

import rescala.events._
import rescala._
import makro.SignalMacro.{SignalM => Signal}



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
    time() = time.get + 1
  }
  

}





















