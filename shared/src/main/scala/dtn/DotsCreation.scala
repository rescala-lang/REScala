package dtn

import kofre.time.{Dots, Time}
import kofre.base.Uid
import scala.util.Random



object DotsCreation {
  val uid_pool: List[Uid] = List("A1", "B2", "C3", "D4", "E5", "F6", "G7", "H8", "I9", "J10").map(Uid.predefined)

  def generate_pseudo_random_dots(): Dots = {
    val rand = Random(28394057)

    var dots = Dots.empty

    for (_ <- 0 to 7) {
      val uid: Uid = uid_pool(rand.nextInt(uid_pool.size))

      val current_time: Long = System.currentTimeMillis()
      val earliest_time: Long = current_time - (1000 * 60 * 60 * 24 * 365 * 4)

      for (_ <- 0 to 20) {
        dots = dots.add(uid, rand.between(earliest_time, current_time))
      }
    }

    dots
  }
}


