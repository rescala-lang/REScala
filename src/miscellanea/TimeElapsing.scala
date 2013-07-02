package miscellanea

import react._
import macro.SignalMacro.{SignalM => Signal}


object TimeElapsing extends App {
  
  println("start!")
  
  val second = Var(0)
  val minute = Signal{ second() % 60 }
  val hour = Signal{ minute() % 60 }
  val day = Signal{ hour() % 24 }
  

  while(true){
    println((second.getValue, minute.getVal, hour.getVal, day.getVal))
    second.setVal(second.getValue + 1)
  }
  
}



