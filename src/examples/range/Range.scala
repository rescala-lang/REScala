package examples.range

import react._
import macro.SignalMacro.{SignalM => Signal}

class Range1(protected var _start : Int, protected var _length : Int){
  protected var _end = _start + _length // backs up end
  
  // getters and setters, maintaining correct state
  def start = _start
  def end = _end
  def length = _length  
  def start_=(s : Int){
    _start = s
    _end = _start + _length
  }
  def end_=(e : Int) {
    _end = e
    _length = e - _start
  }
  def length_=(e : Int){
    _length = e
    _end = _start + _length
  }  
}

class Range2(var start : Int, var length : Int){
  def end = start + length
  def end_=(e : Int) = length = e - start  
}

class Range3(val start : Var[Int], val length : Var[Int]) {
	// end is a signal, leading to transparent caching
	lazy val end = Signal { start() + length()}
	lazy val last = Signal { end() - 1 }
	def end_=(e : Int) = length() = e - start()
	
	// invariant
	length.toSignal.changed += {(x : Int) => 
	  if(x < 0) throw new IllegalArgumentException}
	
	// convenience functions
	def contains(number : Int) = number > start() && number < end()
	def contains(other : Range3) = start() < other.start() && end() > other.end()
	def intersects(other : Range3) = contains(other.start()) || contains(other.end())
}


// immutable range
class Range(val start : Int, val length : Int) {
	lazy val end = start + length
}