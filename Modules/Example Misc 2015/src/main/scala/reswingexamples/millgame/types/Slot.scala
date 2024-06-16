package reswingexamples.millgame.types

sealed abstract class Slot { def other: Slot }
case object Empty extends Slot { val other = Empty }
case object Black extends Slot { val other = White }
case object White extends Slot { val other = Black }

final case class SlotIndex(index: Int) extends AnyVal {
  override def toString = "#" + index
}
