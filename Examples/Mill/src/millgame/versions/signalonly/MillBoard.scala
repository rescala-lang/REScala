package millgame.versions.signalonly

import millgame._
import millgame.types._

import makro.SignalMacro.{SignalM => Signal}

import rescala.Signal
import rescala.Var
import rescala.events.Event

class MillBoard {
    /* wrap stones Var, to have the same interface as other versions */
    def stones = stonesVar.get
  
	/* spiral-indexed board slots, starting innermost lower left, going clockwise */
    val stonesVar: Var[Vector[Slot]] = Var(Vector.fill(24)(Empty)) //#VAR
	
	/* slots by the 16 lines of the game */
	val lines = Signal { //#SIG
      MillBoard.lines map { _ map { slot => stonesVar()(slot.index) } }
    }
	
	/* lines mapped to owners */
	val lineOwners: Signal[Vector[Slot]] = Signal { //#SIG
	  (lines() map { line =>
	    if(line forall { _ == line.head }) line.head else Empty
	  }).toVector
	}
	
	/* observers */
	var millClosedListerer: List[(Slot => Unit)] = Nil
	var numStonesChangedListener: List[(((Slot, Int)) => Unit)] = Nil
	def addMillClosedListener(l: (Slot => Unit)) = millClosedListerer ::= l
	def addStonesChangedListenerlistener(l: (((Slot, Int)) => Unit)) = numStonesChangedListener ::= l
	def notifyMillClosed(s: Slot) = millClosedListerer.foreach(_(s))
	def notifyStonesChanged(c: (Slot, Int)) = numStonesChangedListener.foreach(_(c))
	
	/* access slot state by index */
	def apply(slot: SlotIndex) = stonesVar.get(slot.index)	
	def update(slot: SlotIndex, color: Slot) = {
	  stonesVar.set(stonesVar.get.updated(slot.index, color))
	}
	
	val color: Signal[SlotIndex => Slot] = Signal { //#SIG
	  val stones = stonesVar()
	  slot => stones(slot.index)
	}
	
	/* several test signals*/
	val canPlace: Signal[SlotIndex => Boolean] = Signal { //#SIG
	  val col = color()
	  slot => col(slot) == Empty }
	val canRemove: Signal[SlotIndex => Boolean] = Signal { //#SIG
	  val col = color()
	  slot => col(slot) != Empty }
	val canJump: Signal[(SlotIndex, SlotIndex) => Boolean] = Signal { //#SIG
	  val removeAllowed = canRemove()
	  val placeAllowed = canPlace()
	  (from, to) => removeAllowed(from) && placeAllowed(to) }
	val canMove: Signal[(SlotIndex, SlotIndex) => Boolean] = Signal { //#SIG
	  val jumpAllowed = canJump()
	  (from, to) => jumpAllowed(from, to) && MillBoard.isConnected(from, to) }

	def place(slot: SlotIndex, color: Slot) {
	  val previousOwners = lineOwners.get
	  this(slot) = color
	  val mill = previousOwners zip lineOwners.get collectFirst {
	    case (a, b) if a != b => b
	  }
	  
	  // possible fire mill closed event
	  mill foreach { notifyMillClosed(_) }
	  
	  // fire numStonesChanged event
	  notifyStonesChanged((color, numStones.get(color)))
	}
	
	def remove(slot: SlotIndex) = {
	  val color = this(slot)
	  this(slot) = Empty

	  // fire numStonesChanged event
	  notifyStonesChanged((color, numStones.get(color)))
	}
	
	def move(from: SlotIndex, to: SlotIndex) = { 
	  val color = this(from)
	  this(from) = Empty
	  place(to, color)
	}
	
	val possibleMoves: Signal[Seq[(SlotIndex, SlotIndex)]] = Signal { //#SIG
	  MillBoard.indices flatMap { from =>
	    MillBoard.indices collect { case to if canMove()(from, to) => from -> to }
	  }
	}
	
	val possibleJumps: Signal[Seq[(SlotIndex, SlotIndex)]] = Signal { //#SIG
	  MillBoard.indices flatMap { from =>
	    MillBoard.indices collect { case to if canJump()(from, to) => from -> to }
	  }
	}
	
	val numStones: Signal[(Slot => Int)] = Signal { //#SIG
	  val stones = stonesVar()
	  (color: Slot) => stones.count(_ == color)
	}
}
