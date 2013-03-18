package texteditor.imperative

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

class GapBuffer {
  private var buf = new Array[Char](0)
  private var size = 0
  private var offset = 0
  
  def caret = offset
  def caret_=(value: Int) {
    if (value >= 0 && value <= size) {
      val (cur, prev) = (value, caret)
      val (post, dist) = (buf.length - size + prev, math.abs(cur - prev))
      val (src, dest) = if (prev < cur) (post, prev) else (cur, post - dist)
      
      Array.copy(buf, src, buf, dest, dist)
      offset = cur
    }
  }
  
  def iterator = (new CharacterIterator(buf, size, caret)): Iterator[Char]
  
  def length = size
  
  def apply(i: Int) = buf(if (i >= caret) i + (buf.length - size) else i)
  
  def insert(str: String) {
    if (size + str.length > buf.length)
      expand(size + str.length)
    
    val post = buf.length - size + caret
    str.copyToArray(buf, post - str.length, str.length);
    size += str.length
  }
  
  def remove(count: Int) {
    size -= math.min(count, size - caret)
  }
  
  private def expand(minsize: Int) {
    val postlength = size - caret
    val newlength = 2 * minsize
    var newbuf = new Array[Char](newlength)
    
    Array.copy(buf, 0, newbuf, 0, caret)
    Array.copy(buf, buf.length - postlength, newbuf, newbuf.length - postlength, postlength)
    
    buf = newbuf
  }
}
