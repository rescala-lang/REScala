package millgame.versions.signalonly
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
  
    /* wrap stones Var, to have the same interface as other versions */
    def stones = stonesVar.getVal
  
	/* spiral-indexed board slots, starting innermost lower left, going clockwise */
    val stonesVar: Var[Vector[Slot]] = Var(Vector.fill(24)(Empty)) //#VAR
	
	/* slots by the 16 lines of the game */
	val lines = Signal { //#SIG
      MillBoard.Lines.map(line => line.map(stonesVar()(_))) 
    }
	
	/* lines mapped to owners */
	val lineOwners: Signal[Vector[Slot]] = Signal { //#SIG
	  lines().map(line => if(line.forall(_ == line.head)) line.head else Empty).toVector
	}
	
	val possibleMoves: Signal[Seq[(Int, Int)]] = Signal { //#SIG
	  val range = 0 until stonesVar().size
	  range flatMap { from =>
	    range collect { case to if canMove(from, to) => (from, to) }
	  }
	}
	
	val possibleJumps: Signal[Seq[(Int, Int)]] = Signal { //#SIG
	  val range = 0 until stonesVar().size
	  range flatMap { from =>
	    range collect { case to if canJump(from, to) => (from, to) }
	  }
	}
	
	/* observers */
	var millClosedListerer: List[(Slot => Unit)] = Nil
	var numStonesChangedListener: List[(((Slot, Int)) => Unit)] = Nil
	def addMillClosedListener(l: (Slot => Unit)) = millClosedListerer ::= l
	def addStonesChangedListenerlistener(l: (((Slot, Int)) => Unit)) = numStonesChangedListener ::= l
	def notifyMillClosed(s: Slot) = millClosedListerer.foreach(_(s))
	def notifyStonesChanged(c: (Slot, Int)) = numStonesChangedListener.foreach(_(c))
	
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
		(math.abs(i - j) == 1 && math.max(i, j) % 8 != 0) || 
		(math.abs(i - j) == 8 && i % 2 != 0) ||
		(math.abs(i - j) == 7 && math.min(i, j) % 8 == 0)
		)

	def place(i: Int, color: Slot) {
	  val previousOwners = lineOwners.getValue
	  this(i) = color
	  val mill = previousOwners.zip(lineOwners.getValue).collectFirst { case (a, b) if a != b => b }
	  
	  // possible fire mill closed event
	  mill.foreach(notifyMillClosed(_))
	  
	  // fire numStonesChanged event
	  notifyStonesChanged((color, numStones.getValue(color)))
	}
	
	def remove(i: Int) = {
	  val color = stones(i)
	  this(i) = Empty

	  // fire numStonesChanged event
	  notifyStonesChanged((color, numStones.getValue(color)))
	}
	
	def move(i: Int, j: Int) = { 
	  val color = stones(i)
	  this(i) = Empty
	  place(j, color)
	}
	
	val numStones: Signal[(Slot => Int)] = Signal { //#SIG
	  val stones = stonesVar()
	  (color: Slot) => stones.count(_ == color)
	}
}

object Test extends Application {
  	val mill = new MillBoard
  	mill(0) = Black
  	mill(1) = Black
  	mill(2) = Black
}