package examples.scalatests

class CyclicData {

  // cyclic data structures
  class Ref(o: => Ref) { lazy val other = o }
  class Test {
    val a: Ref = new Ref(b)
    val b: Ref = new Ref(a)
  }
  val t = new Test
  println(t.a.other)
}