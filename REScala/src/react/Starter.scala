package react



object Starter extends App {

  println("start!")
  
  val v1 = Var(1)
  val v2 = Var(2)
  val v3 = Var(3)
  
  val s = Signal(List(v1,v2,v3)){  println("evaluated");
    v1.getValue + 
    v2.getValue + 
    v3.getValue + 1
  }


  println(s.getVal)
  
  
}






