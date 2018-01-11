package rescala.fullmv
import java.util

trait TransactionSpanningTreeNode[T] {
  val txn: T
  def childCount(): Int
  def iterator(): java.util.Iterator[_ <: TransactionSpanningTreeNode[T]]
  def map[B](f: T => B): CaseClassTransactionSpanningTreeNode[B] = {
    val it = iterator()
    CaseClassTransactionSpanningTreeNode(f(txn), Array.fill(childCount()) { it.next().map(f) })
  }
}

case class CaseClassTransactionSpanningTreeNode[T](txn: T, children: Array[CaseClassTransactionSpanningTreeNode[T]]) extends TransactionSpanningTreeNode[T] {
  override def childCount(): Int = children.length
  override def iterator(): java.util.Iterator[_ <: TransactionSpanningTreeNode[T]] = new java.util.Iterator[CaseClassTransactionSpanningTreeNode[T]] {
    var current = 0
    override def hasNext: Boolean = current < children.length
    override def next(): CaseClassTransactionSpanningTreeNode[T] = {
      val e = children(current)
      current += 1
      e
    }
  }
}

trait IMutableTransactionSpanningTreeNode[T] extends TransactionSpanningTreeNode[T] {
  def addChild(child: MutableTransactionSpanningTreeNode[T]): Unit
}

class MutableTransactionSpanningTreeNode[T](val txn: T) extends java.util.ArrayList[MutableTransactionSpanningTreeNode[T]] with IMutableTransactionSpanningTreeNode[T] {
  override def childCount(): Int = super.size()
  override def addChild(e: MutableTransactionSpanningTreeNode[T]): Unit = super.add(e)
}
class MutableTransactionSpanningTreeRoot[T](val txn: T) extends IMutableTransactionSpanningTreeNode[T] {
  @volatile var children: Array[MutableTransactionSpanningTreeNode[T]] = new Array(6)
  @volatile var size: Int = 0

  override def addChild(child: MutableTransactionSpanningTreeNode[T]): Unit = {
    if(children.length == size) {
      val newChildren = new Array[MutableTransactionSpanningTreeNode[T]](children.length + (children.length >> 1))
      System.arraycopy(children, 0, newChildren, 0, size)
      children = newChildren
    }
    children(size) = child
    size += 1
  }

  override def childCount(): Int = size
  override def iterator(): util.Iterator[MutableTransactionSpanningTreeNode[T]] = new util.Iterator[MutableTransactionSpanningTreeNode[T]] {
    private var idx = 0
    override def next(): MutableTransactionSpanningTreeNode[T] = {
      val r = children(idx)
      idx += 1
      r
    }
    override def hasNext: Boolean = idx < size
  }
}
