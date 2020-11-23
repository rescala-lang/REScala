import rescala.collectionsDefault._

/** @author gerizuna
  * @since 02.07.19
  */
object Main {
  val z    = SeqSource[Int]
  val max  = z.max
  val min  = z.min
  val size = z.size

  z.add(1)
  z.add(2)
  z.add(3)
  z.add(4)
  z.add(5)
  z.add(3)
  z.add(3)
  z.remove(5)

  println(max.now)

}
