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

case class CaseClassTransactionSpanningTreeNode[T](txn: T, children: Array[CaseClassTransactionSpanningTreeNode[T]])
    extends TransactionSpanningTreeNode[T] {
  override def childCount(): Int = children.length
  override def iterator(): java.util.Iterator[_ <: TransactionSpanningTreeNode[T]] =
    new java.util.Iterator[CaseClassTransactionSpanningTreeNode[T]] {
      var current                   = 0
      override def hasNext: Boolean = current < children.length
      override def next(): CaseClassTransactionSpanningTreeNode[T] = {
        val e = children(current)
        current += 1
        e
      }
    }
}

class MutableTransactionSpanningTreeNode[T](val txn: T) extends TransactionSpanningTreeNode[T] {
  @volatile var children: Array[MutableTransactionSpanningTreeNode[T]] = new Array(6)
  @volatile var size: Int                                              = 0

  // callers must execute mutually exclusive.
  // updates size after the update is complete; if the array reference is replaced, the original are kept intact.
  // readers can thus safely read the set of children concurrently by first reading size, then reading the array reference, and then iterating.
  // iterator() is thread-safe following this pattern in that first calling hasNext() compares against size, and calling next() afterwards is then safe.
  // concurrent writes may or may not be visible, clients must manually implement according synchronization if required.
  def addChild(child: MutableTransactionSpanningTreeNode[T]): Unit = {
    if (children.length == size) {
      val newChildren = new Array[MutableTransactionSpanningTreeNode[T]](children.length + (children.length >> 1))
      System.arraycopy(children, 0, newChildren, 0, size)
      children = newChildren
    }
    children(size) = child
    size += 1
  }

  override def childCount(): Int = size
  override def iterator(): util.Iterator[MutableTransactionSpanningTreeNode[T]] =
    new util.Iterator[MutableTransactionSpanningTreeNode[T]] {
      private var idx = 0
      override def next(): MutableTransactionSpanningTreeNode[T] = {
        val r = children(idx)
        idx += 1
        r
      }
      override def hasNext: Boolean = idx < size
    }
}
