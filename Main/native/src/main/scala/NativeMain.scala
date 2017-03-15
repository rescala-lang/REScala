import rescala._

object NativeMain {

  def main(args: Array[String]): Unit = {
    val v = Var(5)
    v.map(_ + 10)
    v.observe(println)

    v.set(100)
  }
}
