package examples.timeElapsing

import react._
import macro.SignalMacro.{SignalM => Signal}
import react.events._



object AdvancedTimeElapsing extends App {
  
  println("start!")
  
  val tick = new ImperativeEvent[Unit]()
  
  val numTics = tick.count
  val seconds = Signal{ numTics() % 60 }  
  val minutes = Signal{ seconds.changedTo(0).count() % 60 }
  val hours = Signal{ minutes.changedTo(0).count() % 24 } 
  val days = hours.changedTo(0).count
  

  while(true){
    Thread.sleep(0)
    println((seconds.getVal, minutes.getVal, hours.getVal, days.getVal))
    tick()
  }
  
}



