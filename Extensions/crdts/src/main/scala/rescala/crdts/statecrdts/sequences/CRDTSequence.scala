package rescala.crdts.statecrdts.sequences

import rescala.crdts.statecrdts.StateCRDT
import rescala.crdts.statecrdts.sets.StateCRDTSet

import scala.collection.AbstractIterator
import scala.collection.immutable.HashMap

trait CRDTSequence[A] {
  type valueType = List[A]
  type payloadType
  type selfType

  def payload: payloadType

  def vertices: StateCRDTSet[ValueVertex[A]]

  def edges: HashMap[Vertex, Vertex]

  def contains(v: Vertex): Boolean = v match {
    case `startVertex` => true
    case `endVertex` => true
    case v: ValueVertex[A] => vertices.contains(v)
  }

  def containsValue(a: A): Boolean = vertices.value.map(_.value).contains(a)

  def before(u: Vertex, v: Vertex): Boolean = u match {
    case `startVertex` => true
    case `endVertex` => false
    case u: ValueVertex[A] => edges(u) == v || before(edges(u), v)
    case _ => throw new IllegalArgumentException(s"CRDTSequence does not contain Vertex $u!")
  }

  def successor[A1 >: A](v: Vertex): Vertex = v match {
    case `endVertex` =>
      throw new IllegalArgumentException("There is no successor to the end node!")
    case _ =>
      if (edges.contains(v)) edges(v) match {
        case `endVertex` => Vertex.end
        case u: ValueVertex[A1] =>
          if (contains(u)) u
          else successor(u)
      }
      else throw new IllegalArgumentException(s"CRDTSequence does not contain $v")
  }

  def addRight[F](position: Vertex, a: A): selfType = addRight(position, new ValueVertex[A](a, Vertex.genTimestamp))

  /**
    * This method allows insertions of any type into the RGA. This is used to move the start and end nodes
    *
    * @param position the vertex specifying the position
    * @param v        the vertex to be inserted right to position
    * @return A new RAG containing the inserted element
    */
  def addRight(position: Vertex, v: ValueVertex[A])(implicit
                                                    stateCRDT: StateCRDT[valueType, selfType]): selfType =
    position match {
      case `endVertex` =>
        throw new IllegalArgumentException("Cannot insert after end node!")
      case _ => if (edges.contains(position)) {
        val (l, r) = (position, edges(position))
        // Check if the vertex right to us has been inserted after us.  If yes, insert v after the new vertex.
        if (r.timestamp > v.timestamp) addRight(r, v)
        else {
          val newVertices = vertices.add(v)
          val newEdges = edges + (l -> v) + (v -> r)
          stateCRDT.fromPayload((newVertices, newEdges))
        }
      }
      else {
        throw new IllegalArgumentException(s"Insertion failed! CRDTSequence does not contain specified position vertex $position!")
      }
    }

  def append(v: ValueVertex[A]): selfType = {
    val position = if (vertexIterator.nonEmpty) vertexIterator.toList.last else Vertex.start
    addRight(position, v)
  }

  def prepend(v: ValueVertex[A]): selfType = {
    addRight(Vertex.start, v)
  }

  def value: List[A] = iterator.toList

  def iterator: Iterator[A] = vertexIterator.map(v => v.value)

  def vertexIterator: Iterator[ValueVertex[A]] = new AbstractIterator[ValueVertex[A]] {
    var lastVertex: Vertex = `startVertex`

    override def hasNext: Boolean = successor(lastVertex) match {
      case `endVertex` => false
      case _ => true
    }

    override def next(): ValueVertex[A] = {
      successor(lastVertex) match {
        case v: ValueVertex[A] => lastVertex = v; v
      }
    }
  }
}
