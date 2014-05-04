package millgame.versions.events
import millgame.types._
import react.events._

object MillBoard {
  val borders = (0 to 23 by 2) map { init => 
    List.iterate(init, 3) { x =>
      (x + 1) - (if ((x + 1) % ((init / 8 + 1) * 8) == 0) 8 else 0)
    } map { SlotIndex(_) }
  }
  
  val crosses = (1 to 8 by 2) map { List.iterate(_, 3)(_ + 8) map { SlotIndex(_) } }
  
  val lines = borders ++ crosses
  
  val indices = (0 until 24) map { SlotIndex(_) }
  
  def isConnected(from: SlotIndex, to: SlotIndex) = {
    val i = from.index
    val j = to.index
    (math.abs(i - j) == 1 && math.max(i, j) % 8 != 0) || 
    (math.abs(i - j) == 8 && i % 2 != 0) ||
    (math.abs(i - j) == 7 && math.min(i, j) % 8 == 0)
  }
}

class MillBoard {
  
	/* spiral-indexed board slots, starting innermost lower left, going clockwise */
	var stones: Array[Slot] = Array.fill(MillBoard.indices.size)(Empty)
	
	/* slots by the 16 lines of the game */
	def lines = MillBoard.lines map { _ map { slot => stones(slot.index) } }
	
	/* lines mapped to owners */
	def lineOwners = lines map { line =>
	  if(line forall { _ == line.head }) line.head else Empty
	}
	
	/* access slot state by index */
	def apply(slot: SlotIndex) = stones(slot.index)
	
	def numStones(color: Slot) = stones count { _ == color }
	
	val millClosed = new ImperativeEvent[Slot] //#EVT
	
	val numStonesChanged = new ImperativeEvent[(Slot, Int)] //#EVT
	
	def possibleMoves: Seq[(SlotIndex, SlotIndex)] = {
	  MillBoard.indices flatMap { from =>
	    MillBoard.indices collect { case to if canMove(from, to) => from -> to }
	  }
	}
	
	def possibleJumps: Seq[(SlotIndex, SlotIndex)] = {
	  MillBoard.indices flatMap { from =>
	    MillBoard.indices collect { case to if canJump(from, to) => from -> to }
	  }
	}
	
	/* several test methods*/
	def canPlace(slot: SlotIndex) = this(slot) == Empty
	def canRemove(slot: SlotIndex) = this(slot) != Empty
	def canJump(from: SlotIndex, to: SlotIndex) =
	  canRemove(from) && canPlace(to)
	def canMove(from: SlotIndex, to: SlotIndex) =
	  canJump(from, to) && MillBoard.isConnected(from, to)

	def place(slot: SlotIndex, color: Slot) {
	  val previousOwners = lineOwners
	  stones(slot.index) = color
	  
	  val mill = previousOwners zip lineOwners collectFirst {
	    case (a, b) if a != b && b != Empty => b 
	  }
	  
	  // possible fire mill closed event
	  mill foreach { millClosed(_) }
	  
	  // fire numStonesChanged event
	  numStonesChanged(color, numStones(color))
	}
	
	def remove(slot: SlotIndex) = {
	  val color = stones(slot.index)
	  stones(slot.index) = Empty
	  // fire numStonesChanged event
	  numStonesChanged(color, numStones(color))
	}
	
	def move(from: SlotIndex, to: SlotIndex) = { 
	  val color = stones(from.index)
	  stones(from.index) = Empty
	  place(to, color)
	}
}
