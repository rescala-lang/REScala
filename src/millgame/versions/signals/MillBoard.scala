package millgame.versions.signals
import millgame.types._
import react.events._
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}

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
  
    def stones = stonesVar.getVal
  
	/* spiral-indexed board slots, starting innermost lower left, going clockwise */
    val stonesVar: Var[Vector[Slot]] = Var(Vector.fill(24)(Empty))	
	
	/* slots by the 16 lines of the game */
	val lines = Signal {
      MillBoard.Lines.map(line => line.map(stonesVar()(_))) 
    }
	
	/* lines mapped to owners */
	val lineOwners: Signal[Vector[Slot]] = Signal {
	  lines().map(line => if(line.forall(_ == line.head)) line.head else Empty).toVector
	}
	
	
	/* access slot state by index */
	def apply(i: Int) = stonesVar.getVal(i)	
	def update(i: Int, color: Slot) = {
	  stonesVar.setVal(stonesVar.getVal.updated(i, color))
	}
	
	/* several test methods*/
	def canPlace(i: Int) = this(i) == Empty
	def canRemove(i: Int) = this(i) != Empty
	def canJump(i: Int, j: Int) = canRemove(i) && canPlace(j)
	def canMove(i: Int, j: Int) = canJump(i, j) && (
		math.abs(i - j) == 1 || 
		(math.abs(i - j) == 8 && i % 2 != 0 && j % 2 != 0) ||
		math.abs(i - j) == 7 && math.min(i, j) % 8 == 0
		)

	def place(i: Int, color: Slot) = this(i) = color
	
	def remove(i: Int) =  this(i) = Empty
	
	def move(i: Int, j: Int) = {
	  this(j) = this(i)
	  this(i) = Empty
	  /// NOTE: this is an interesting detail: In the signal version, we have to make sure to
	  /// place the new stone FIRST, then delete the old one. Otherwise we might have < 3 stones,
	  /// which gets propagated and triggers the end of the game!
	  /// "move" actually is an atomic operation on the board, and we probably want events
	  /// to trigger fine-grained changes like this
	}
	
	
	/// NOTE: Workaround because change fires even when there is no value change
	val lineOwnersChanged = lineOwners.change && ((c: (Vector[Slot], Vector[Slot])) => c._2 != c._1)
	val millOpenedOrClosed = lineOwners.change.map { 
	  change: (Vector[Slot], Vector[Slot]) => 
	  /// NOTE: Workaround because change event fires (null, new) tuple
	  if (change._1 eq null) change._2.find(_ != Empty).get
	  else (change._1 zip change._2).collectFirst {case (old, n) if old != n => n}.get
	}

	val millClosed: Event[Slot] = millOpenedOrClosed && {(_: Slot) != Empty}
	
	val numStones: Signal[(Slot => Int)] = Signal {
	  val stones = stonesVar()
	  (color: Slot) => stones.count(_ == color)
	}
	val blackStones = Signal { numStones()(Black) }
	val whiteStones = Signal { numStones()(White) }
	val numStonesChanged: Event[(Slot, Int)] = 
	  blackStones.changed.map( (Black, _: Int)) || whiteStones.changed.map((White, _: Int))
}


object Test extends Application {
  	val mill = new MillBoard
  	mill(0) = Black
  	mill(1) = Black
  	mill(2) = Black
}