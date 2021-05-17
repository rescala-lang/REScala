package rescala

import rescala.scheduler.levelbased.LevelBasedSchedulers._

object Test {
  def main(args: Array[String]): Unit = {
    val a = Var("hello redotty")
    val b = Signals.static(a)(st => st.dependStatic(a) + " appendage")
    b.observe(println)

    a.set("ahahaha")
  }
}
