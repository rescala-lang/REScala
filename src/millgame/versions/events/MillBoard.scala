package millgame.versions.events
import millgame.types._
import react.events._

object MillBoard {
  
  	val Borders = (0 to 23 by 2).map(init => 
	  	List.iterate(init, 3)(x => (x + 1) -
	  	   (if ((x + 1) % ( (init / 8 + 1) * 8) == 0) 8 else 0)
	  	)
	)
	val Crosses = (1 to 8 by 2).map(List.iterate(_, 3)(x => x + 8))
	
	val Lines = Borders ++ Crosses
}

class MillBoard {
  
	/* spiral-indexed board slots, starting innermost lower left, going clockwise */
	var stones: Array[Slot] = Array.fill(24)(Empty)
	
	/* slots by the 16 lines of the game */
	def lines = MillBoard.Lines.map(line => line.map(stones(_)))
	
	/* lines mapped to owners */
	def lineOwners = lines.map(line => if(line.forall(_ == line.head)) line.head else Empty)
	
	/* access slot state by index */
	def apply(i: Int) = stones(i)
	
	def numStones(color: Slot) = stones.count(_ == color)
	
	val millClosed = new ImperativeEvent[Slot] //#EVT
	
	val numStonesChanged = new ImperativeEvent[(Slot, Int)] //#EVT
	
	def possibleMoves: Seq[(Int, Int)] = {
	  val range = 0 until stones.size
	  range flatMap { from =>
	    range collect { case to if canMove(from, to) => (from, to) }
	  }
	}
	
	def possibleJumps: Seq[(Int, Int)] = {
	  val range = 0 until stones.size
	  range flatMap { from =>
	    range collect { case to if canJump(from, to) => (from, to) }
	  }
	}
	
	/* several test methods*/
	def canPlace(i: Int) = stones(i) == Empty
	def canRemove(i: Int) = stones(i) != Empty
	def canJump(i: Int, j: Int) = canRemove(i) && canPlace(j)
	def canMove(i: Int, j: Int) = canJump(i, j) && (
		(math.abs(i - j) == 1 && math.max(i, j) % 8 != 0) || 
		(math.abs(i - j) == 8 && i % 2 != 0) ||
		(math.abs(i - j) == 7 && math.min(i, j) % 8 == 0)
		)

	def place(i: Int, color: Slot) {
	  val previousOwners = lineOwners
	  stones(i) = color
	  
	  val mill = previousOwners.zip(lineOwners).collectFirst {
	    case (a, b) if a != b && b != Empty => b 
	  }
	  
	  // possible fire mill closed event
	  mill.foreach(millClosed(_))
	  
	  // fire numStonesChanged event
	  numStonesChanged(color, numStones(color))
	}
	
	def remove(i: Int) = {
	  val color = stones(i)
	  stones(i) = Empty
	  // fire numStonesChanged event
	  numStonesChanged(color, numStones(color))
	}
	
	def move(i: Int, j: Int) = { 
	  val color = stones(i)
	  stones(i) = Empty
	  place(j, color)
	}

}


object Test extends Application {
  	val mill = new MillBoard
  	mill.stones(0) = Black
  	mill.stones(1) = Black
  	mill.stones(2) = Black
  	
  	println(mill.lineOwners)
}