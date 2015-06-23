package rescala

trait Build {
  type A
  def makeA: A
  def takeA(a: A): Unit
}

class Test[B <: Build](val b: B) {

  val anA: b.A = b.makeA

  b.takeA(anA)
}

object IntB extends Build {
override type A = Int
override def makeA: A = 1
override def takeA(a: A): Unit = println(a + 1)
}

object Main extends App {
  val t = new Test(IntB)
  t.b.takeA(6)

  println("done")
}
