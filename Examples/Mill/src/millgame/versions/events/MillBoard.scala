package millgame.versions.events

import millgame._
import millgame.types._
import rescala.events.ImperativeEvent

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
