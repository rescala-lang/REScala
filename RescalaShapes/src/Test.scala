import scala.events.behaviour.Signal
import scala.events.behaviour.Var
object Test {

  def main(args: Array[String]): Unit = {
    val a = new Var(List(1, 2, 3))
    val sum = Signal { "lenght is %d".format(a().size) }

    sum.changed += (newVal => println(newVal))

    //println(sum.getValue)
    //println(sum.getValue)
    a() = List(1, 2, 3, 4, 5)
  }

}