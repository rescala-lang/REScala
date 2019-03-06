package rescala.crdts.statecrdts.sequences

import rescala.crdts.statecrdts.sets.StateCRDTSet

import scala.collection.AbstractIterator

trait CRDTSequence[A] {
  type ValueT = List[A]
  type PayloadT
  type SelfT

  val vertices: StateCRDTSet[Vertex]

  val edges: Map[Vertex, Vertex]

  val values: Map[Vertex, A]

  def fromPayload(payload: PayloadT): SelfT

  def contains(v: Vertex): Boolean = v match {
    case Vertex.start => true
    case Vertex.end   => true
    case v: Vertex    => vertices.contains(v)
  }

  def before(u: Vertex, v: Vertex): Boolean = u match {
    case Vertex.start => true
    case Vertex.end   => false
    case u: Vertex    => edges(u) == v || before(edges(u), v)
    case _            => throw new IllegalArgumentException(s"CRDTSequence does not contain Vertex $u!")
  }

  def successor(v: Vertex): Vertex = {
//    println(s"computing successor of $v in ${edges.keySet}")
    v match {
      case Vertex.end =>
        throw new IllegalArgumentException("There is no successor to the end node!")
      case _          => {
//      logger.debug(s"Searching successor of $v. Edges: $edges")
        println(s"searching for $v in $edges")
        if (edges.contains(v)) {
          edges(v) match {
            case Vertex.end => Vertex.end
            case u: Vertex  =>
              if (contains(u)) u
              else successor(u)
          }
        }
        else throw new IllegalArgumentException(s"CRDTSequence does not contain $v")
      }
    }
  }

  def addRight(position: Vertex, a: A): SelfT = addRight(position, Vertex.fresh(), a)

  /**
    * This method allows insertions of any type into the RGA. This is used to move the start and end nodes
    *
    * @param position the vertex specifying the position
    * @param v        the vertex to be inserted right to position
    * @return A new RAG containing the inserted element
    */
  def addRight(position: Vertex, v: Vertex, value: A): SelfT =
    position match {
      case Vertex.end =>
        throw new IllegalArgumentException("Cannot insert after end node!")
      case _          =>
        if (edges.contains(position)) {
          val (l, r) = (position, edges(position))
          // Check if the vertex right to us has been inserted after us.  If yes, insert v after the new vertex.
          if (r.timestamp > v.timestamp) addRight(r, v, value)
          else {
            val newVertices = vertices.add(v)
            val newEdges = edges + (l -> v) + (v -> r)
            fromPayload((newVertices, newEdges).asInstanceOf[PayloadT])
          }
        }
        else {
          throw new IllegalArgumentException(s"Insertion failed! CRDTSequence does not contain specified position vertex $position!")
        }
    }

  def append(value: A): SelfT = {
    val position = if (vertexIterator.nonEmpty) vertexIterator.toList.last else Vertex.start
    addRight(position, value)
  }

  def prepend(value: A): SelfT = addRight(Vertex.start, value)


  def value: List[A] = {
    println(s"iterating")
    val list = iterator.toList
    println(s"iterating done")
    list
  }

  def iterator: Iterator[A] = vertexIterator.map(v => values(v))

  def vertexIterator: Iterator[Vertex] = new AbstractIterator[Vertex] {
    var lastVertex: Vertex = Vertex.start

    override def hasNext: Boolean = successor(lastVertex) match {
      case Vertex.end => false
      case _          => true
    }

    override def next(): Vertex = {
      successor(lastVertex) match {
        case v: Vertex => lastVertex = v; v
        case _         => throw new NoSuchElementException(
          "Requesting iterator value after Vertex.end!")
      }
    }
  }
}
