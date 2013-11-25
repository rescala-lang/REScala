package examples.timeElapsing

import react._
import macro.SignalMacro.{SignalM => Signal}




object TimeElapsing extends App {
  
  println("start!")
  
  val tick = Var(0)
  val second = Signal{ tick() % 60 }
  val minute = Signal{ tick()/60 % 60 }
  val hour = Signal{ tick()/(60*60) % (60*60) }
  val day = Signal{ tick()/(60*60*24) % (60*60*24) } 
  
  // Note, day is still circular. At some point day==0 again
  // What if one wants that minutes depend on seconds ?

  while(true){
    Thread.sleep(0)
    println((second.getVal, minute.getVal, hour.getVal, day.getVal))
    tick.setVal(tick.getVal + 1)
  }
  
}



