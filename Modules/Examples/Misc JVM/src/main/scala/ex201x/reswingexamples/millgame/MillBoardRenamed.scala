package ex201x.reswingexamples.millgame

import ex201x.reswingexamples.millgame.types.SlotIndex

object MillBoardRenamed {
  val borders = (0 to 23 by 2) map { init =>
    List.iterate(init, 3) { x =>
      (x + 1) - (if (x + 1) % ((init / 8 + 1) * 8) == 0 then 8 else 0)
    } map { SlotIndex(_) }
  }

  val crosses = (1 to 8 by 2) map { List.iterate(_, 3)(_ + 8) map { SlotIndex(_) } }

  val lines = borders ++ crosses

  val indices = (0 until 24) map { SlotIndex(_) }

  def isConnected(from: SlotIndex, to: SlotIndex) = {
    val i = from.index
    val j = to.index
    // TODO: scalafmt seems to have a weird bug for the next lines …
    (math.abs(i - j) == 1 && math.max(i, j)              % 8 != 0) ||
    (math.abs(i - j) == 8 && i              % 2 != 0) ||
    (math.abs(i - j) == 7 && math.min(i, j) % 8 == 0)
  }
}
