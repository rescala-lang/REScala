package examples.timeElapsing

import react._
import macro.SignalMacro.{SignalM => Signal}
import react.events._



object AdvancedTimeElapsing extends App {
  
  println("start!")
  
  val tick = new ImperativeEvent[Unit]()
  
  val numTics = tick.count
  val seconds = Signal{ numTics() % 60 }
  
  val tmpMinutes = seconds.changedTo(0).count
  val minutes = Signal{ tmpMinutes() % 60 }

  val tmpHours = minutes.changedTo(0).count
  val hours = Signal{ tmpHours() % 24 } 
  
  val days = hours.changedTo(0).count
  

  while(true){
    Thread.sleep(0)
    println((seconds.getVal, minutes.getVal, hours.getVal, days.getVal))
    tick()
  }
  
}



