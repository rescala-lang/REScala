package texteditor.reactive

import scala.events.behaviour.Var
import scala.events.behaviour.Signal


/**
 * TODO: add some comments to these classes 
 */
class CharacterIterator(buf: Array[Char], count: Int, caret: Int) extends Iterator[Char] {
  private var b = 0
  private var c = 0
  
  override def hasDefiniteSize = true
  override def size = count
  
  def hasNext = { c < count }
  def next = {
    if (b == caret)
      b += buf.length - count
    val ch = buf(b)
    b += 1
    c += 1
    ch
  }
}

/**
 * TODO: add some comments to these classes 
 * TODO: can you elaborate a bit on what is going on here ? 
 * In particular I'm referring to the Arry.copy thing :)
 */
class GapBuffer {
  private var buf = new Array[Char](0)
  private val size = new Var(0)
  
  val caret = new Var(0) {
    override def update(value: Int) {
      if (value >= 0 && value <= size()) {
        val (cur, prev) = (value, getValue)
        val (post, dist) = (buf.length - size() + prev, math.abs(cur - prev))
        val (src, dest) = if (prev < cur) (post, prev) else (cur, post - dist)
        
        Array.copy(buf, src, buf, dest, dist)
        super.update(cur)
      }
    }
  }
  
  val iterable = Signal{
    val (b, s) = (buf, size())
    new Iterable[Char] { def iterator = new CharacterIterator(b, s, caret.getValue) } : Iterable[Char]
  }
  
  val length = Signal { size() }
  
  def apply(i: Int) = buf(if (i >= caret.getValue) i + (buf.length - size.getValue) else i)
  
  def insert(str: String) {
    if (size.getValue + str.length > buf.length)
      expand(size.getValue + str.length)
    
    val post = buf.length - size.getValue + caret.getValue
    str.copyToArray(buf, post - str.length, str.length);
    size() += str.length
  }
  
  def remove(count: Int) {
    size() -= math.min(count, size.getValue - caret.getValue)
  }
  
  private def expand(minsize: Int) {
    val postlength = size.getValue - caret.getValue
    val newlength = 2 * minsize
    var newbuf = new Array[Char](newlength)
    
    Array.copy(buf, 0, newbuf, 0, caret.getValue)
    Array.copy(buf, buf.length - postlength, newbuf, newbuf.length - postlength, postlength)
    
    buf = newbuf
  }
}
