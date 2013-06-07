package examples

import react.Signal
import react.DepHolder
import react.Var


object TimeElapsing extends App {
  
  println("start!")
  
  val second = Var(0)
  val minute = Signal(List(second)){ second.getValue % 60 }
  val hour = Signal(List(minute)){ minute.getVal % 60 }
  val day = Signal(List(hour)){ hour.getVal % 24 }
  

  while(true){
    println((second.getValue, minute.getVal, hour.getVal, day.getVal))
    second.setVal(second.getValue + 1)
  }
  
}



