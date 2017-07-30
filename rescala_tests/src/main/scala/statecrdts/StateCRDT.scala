package statecrdts

import com.typesafe.scalalogging.Logger
import statecrdts.Vertex.{end, start}

import scala.collection.AbstractIterator
import scala.collection.immutable.HashMap

trait StateCRDT {
  type selfType <: StateCRDT
  type valueType
  type payloadType

  /**
    * the public state of the CRDT
    */
  def value: valueType

  /**
    * the internal state of the CRDT (payload)
    */
  def payload: payloadType

  /**
    * Merge this instance with another CRDT instance and return the resulting CRDT.
    *
    * @param c the other CRDT
    * @return a new CRDT instance representing a merge of the two former instances
    */
  def merge(c: StateCRDT): selfType

  def +(crdt: StateCRDT): selfType = merge(crdt)

  override def toString: String = value.toString

  /**
    * Constructs a new instance of this CRDT from a given payload
    *
    * @param payload the initial payload for this crdt
    * @return a new crdt instance with the given payload
    */
  def fromPayload(payload: payloadType): selfType

  def fromValue(value: valueType): selfType
}

trait StateCRDTSet extends StateCRDT {
  type Element

  def add(e: Element): selfType

  def remove(e: Element): selfType

  def contains(e: Element): Boolean
}

/*
trait StateCRDTGraph extends StateCRDTSequence {
  def contains(e: Edge): Boolean
}
*/

case class CIncOnlyCounter(id: String, payload: HashMap[String, Int]) extends StateCRDT {
  type selfType = CIncOnlyCounter
  type valueType = Int
  type payloadType = HashMap[String, Int]

  def value: valueType = payload.values.sum

  def merge(c: StateCRDT): CIncOnlyCounter = c match {
    case CIncOnlyCounter(_, p) => CIncOnlyCounter(id,
      payload.merged(p) {
        case ((k, v1), (_, v2)) => (k, v1 max v2)
      })
  }

  override def fromValue(value: Int): CIncOnlyCounter = CIncOnlyCounter(id, HashMap(id -> value))

  def fromPayload(payload: HashMap[String, Int]): CIncOnlyCounter = CIncOnlyCounter(id, payload)

  def increase = CIncOnlyCounter(id, payload + (id -> (payload(id) + 1)))
}

object CIncOnlyCounter {
  def apply(value: Int): CIncOnlyCounter = {
    val id = genId // assign random id based on host
    CIncOnlyCounter(id, HashMap(id -> value))
  }
}

/**
  * Implementation of an Observed-Remove Set as described by Shapiro et al. (2011)
  *
  * @param payload The internal state of the set, consisting of two sets. One to store entries and their identifiers and one to track removed entries (tombstones).
  * @tparam A The type of the elements stored in this set
  */
//TODO: maybe use a Map[A,List(Identifier)] for entries. This would speed up removes but slow down adds
case class ORSet[A](payload: (Set[(A, Identifier)], Set[Identifier])) extends StateCRDTSet {
  override type selfType = ORSet[A]
  override type valueType = Set[A]
  override type payloadType = (Set[(A, Identifier)], Set[Identifier])
  override type Element = A
  val (entries, tombstones) = payload

  override def merge(c: StateCRDT): ORSet[A] = c match {
    case o: ORSet[A] =>
      val (entries1, tombs1) = payload
      val (entries2, tombs2) = o.payload
      val (entries, tombstones) = (entries1 ++ entries2, tombs1 ++ tombs2)
      fromPayload((entries, tombstones))
  }

  override def fromPayload(payload: payloadType): ORSet[A] = ORSet(payload)

  override def add(a: A): ORSet[A] = {
    ORSet((entries + ((a, genId)), tombstones))
  }

  override def remove(a: A): ORSet[A] = {
    val (_, newTombs) = entries.filter(entry => entry._1 == a).unzip // fetch ids of all instances of the element
    ORSet((entries, tombstones ++ newTombs)) // add them to tombstones
  }

  override def contains(a: A): Boolean = value(a)

  override def value: valueType = {
    val (values, _) = entries.filter(e => !tombstones(e._2)).unzip // filter all entries with tombstones
    values
  }

  override def fromValue(value: Set[A]): ORSet[A] = {
    val entries = value.map((a) => (a, genId))
    new ORSet((entries, Set()))
  }
}

object ORSet {
  def apply[A](values: A*): ORSet[A] = {
    val a = values.map((a) => (a, genId))
    new ORSet((a.toSet, Set()))
  }
}

/**
  * Two phase set where elements can be added and removed but never added again.
  *
  * @param payload The payload consisting of one set for added entries and one set for removed entries (tombstones).
  * @tparam A The type of the elements in the set.
  */
case class TwoPSet[A](payload: (Set[A], Set[A])) extends StateCRDTSet {
  override type Element = A
  override type selfType = TwoPSet[A]
  override type valueType = Set[A]
  override type payloadType = (Set[A], Set[A])
  val (entries, tombstones): (Set[A], Set[A]) = payload

  override def add(e: Element): selfType = TwoPSet((entries + e, tombstones))

  override def remove(e: Element): selfType = if (entries(e)) TwoPSet((entries, tombstones + e)) else this

  override def contains(e: Element): Boolean = entries.contains(e) && !tombstones.contains(e)

  override def value: valueType = entries -- tombstones

  override def merge(c: StateCRDT): selfType = c match {
    case s: TwoPSet[A] =>
      val e = entries ++ s.entries
      val t = tombstones ++ s.tombstones
      new TwoPSet(e, t)
  }

  override def fromPayload(payload: payloadType): selfType = TwoPSet(payload)

  override def fromValue(value: valueType): selfType = TwoPSet((value, Set[A]()))
}

object TwoPSet {
  def apply[A](values: A*): TwoPSet[A] = {
    new TwoPSet((values.toSet, Set()))
  }
}

/*
/**
  * A monotonic directed acyclic graph. New edges can only be added in the same direction as an existing path.
  *
  * @param payload
  * @tparam A
  */
class AddOnlyDAG[A](payload: (Set[A], Set[(A, A)])) extends StateCRDTGraph {
  override type Vertex[_] = A
  override type selfType = AddOnlyDAG[A]
  override type valueType = None.type
  override type payloadType = (Set[Vertex], Set[Edge])

  val (vertices, edges) = payload

  override def contains(v: Vertex[_]): Boolean = vertices.contains(v)

  override def contains(e: Edge): Boolean = edges.contains(e)

  override def before(v: Vertex[_], u: Vertex[_]): Boolean = ???

  edges(v) == u || {
    val reachableVertices: Set[Vertex] = edges.filter(e => (e._1 == v)).unzip._2
    reachableVertices.exists(r => before(r, u))
  }

  override def value: valueType = None

  override def payload: AddOnlyDAG.this.type = ???

  override def merge(c: StateCRDT): AddOnlyDAG.this.type = ???

  override def fromPayload(payload: AddOnlyDAG.this.type): AddOnlyDAG.this.type = ???

  override def fromValue(value: AddOnlyDAG.this.type): AddOnlyDAG.this.type = ???
}
*/


trait StateCRDTSequence extends StateCRDT {
  type Atom
  //type Edge = (Vertex, Vertex[_])
  //type Node = start.type |∨| Vertex[_]

  def contains[A1 >: Atom](v: Vertex[A1]): Boolean

  def before[A1 >: Atom](u: Vertex[A1], v: Vertex[A1]): Boolean
}

trait RemovableSequence extends StateCRDTSequence {
  //val start: (Atom, Int) = (new Atom, -1)
  //val end: (Atom, Int) = (new Atom, -1)

  def remove[A1 >: Atom](v: Vertex[A1]): selfType

}

/**
  * Replicated Growable Array
  *
  * @param payload The payload consist of one TwoPhase Set which stores the vertices, one HashMap which stores the edges
  *                between vertices and one HashMap which stores a Timestamp for each Vertex.
  * @tparam A The type of the elements stored in this array
  */
case class RGA[A](payload: (TwoPSet[Vertex[Any]], HashMap[Vertex[Any], Vertex[Any]], HashMap[Vertex[Any], Timestamp])) extends StateCRDTSequence with RemovableSequence {
  override type Atom = A
  override type selfType = RGA[A]
  override type valueType = List[A]
  override type payloadType = (TwoPSet[Vertex[Any]], HashMap[Vertex[Any], Vertex[Any]], HashMap[Vertex[Any], Timestamp])
  val logger: Logger = Logger[RGA[A]]
  val (vertices, edges, timestamps): ((TwoPSet[Vertex[Any]], HashMap[Vertex[Any], Vertex[Any]], HashMap[Vertex[Any], Timestamp])) = payload

  override def contains[A1 >: A](v: Vertex[A1]): Boolean = v match {
    case `start` => true
    case `end` => true
    case v: Vertex[A] => vertices.contains(v)
  }

  override def before[A1 >: A](u: Vertex[A1], v: Vertex[A1]): Boolean = u match {
    case `end` => false
    case `start` => true
    case _ => edges(u) == v || before(edges(u), v)
  }

  def successor[A1 >: A](v: Vertex[A1]): Vertex[A1] = v match {
    case `end` =>
      logger.warn("There is no successor to the end node! ")
      v
    case _ =>
      val u: Vertex[A1] = edges(v).asInstanceOf[Vertex[A1]]
      if (vertices.contains(u)) u
      else successor(u)
  }

  //def genTimestamp = timestamps.values.max + 1
  def genTimestamp: Timestamp = System.currentTimeMillis

  def addRight[A1 >: A](position: Vertex[A1], a: A): RGA[A] = addRight(position, new Vertex(a))

  def addRight[A1 >: A](position: Vertex[A1], v: Vertex[A]): RGA[A] = insert(position, v)

  override def remove[A1 >: A](v: Vertex[A1]): RGA[A] = RGA((vertices.remove(v), edges, timestamps))

  override def value: valueType = iterator.toList

  def iterator: Iterator[A] = new AbstractIterator[A] {
    var lastVertex: Vertex[_] = start

    override def hasNext: Boolean = successor(lastVertex) match {
      case `end` => false
      case _ => true
    }

    override def next(): A = {
      val suc: Vertex[_] = successor(lastVertex)
      lastVertex = suc
      suc.value.asInstanceOf[A]
    }
  }

  override def merge(c: StateCRDT): RGA[A] = c match {
    case r: RGA[A] =>
      val mergedTimestamps = timestamps.merged(r.timestamps) {
        // merge timestamps
        case ((v, t1), (_, t2)) => (v, t1 max t2)
      }
      val newVertices = vertices.merge(r.vertices) // merge vertices

      // find conflicting edges
      val (conflictingEdges, rest): (HashMap[Vertex[Any], Vertex[Any]], HashMap[Vertex[Any], Vertex[Any]]) = r.edges.partition(e => edges.contains(e._1) && edges(e._1) != e._2)

      val s = new RGA[A]((newVertices, edges ++: rest, mergedTimestamps)) // construct new RGA

      conflictingEdges.foldLeft(s) {
        // resolve conflicting edges
        case (s: RGA[A], (u: Vertex[Any], v: Vertex[Any])) =>
          // find the right place for our insertion
          var place = u
          while (s.timestamps(v) < s.timestamps(s.edges(place))) place = successor(place)
          s.insert(place, v)
      }
  }

  // TODO: Implement Vertex[_] Iterator

  override def fromPayload(payload: payloadType): RGA[A] = RGA(payload)

  override def fromValue(value: valueType): RGA[A] = {
    val emptyPayload: payloadType = (TwoPSet[Vertex[Any]](start, end), HashMap[Vertex[Any], Vertex[Any]](start -> end), HashMap[Vertex[Any], Timestamp](start -> -1, end -> 0))
    val newRGA: RGA[A] = fromPayload(emptyPayload)

    value.reverse.foldLeft(newRGA) {
      case (r: RGA[A], a) => r.addRight(start, a)
    }
  }

  /**
    * This method allows insertions of any type into the RAG. This is used to move the start and end nodes
    *
    * @param position the vertex specifying the position
    * @param v        the vertex to be inserted right to position
    * @tparam A1 type of both the inserted vertex and the vertex specifying the position
    * @return A new RAG containing the inserted element
    */
  private def insert[A1 >: A](position: Vertex[A1], v: Vertex[A1]): RGA[A] = position match {
    case `end` => logger.error("Cannot insert after end node!"); this
    case _ => if (vertices.contains(position)) {
      val (l, r) = (position, successor(position))
      val timestamp: Timestamp = genTimestamp
      val newVertices = vertices.add(v)
      val newEdges = edges + (l -> v) + (v -> r)
      val newTimestamps = timestamps + (v -> timestamp)
      new RGA((newVertices, newEdges, newTimestamps))
    }
    else {
      logger.error(s"Insertion failed! RGA does not contain specified position vertex $position!")
      this
    }
  }
}

object RGA {
  def apply[A](values: A*): RGA[A] = {
    val r = RGA[A]()
    r.fromValue(values.toList)
  }

  def apply[A](): RGA[A] = empty

  def empty[A]: RGA[A] = new RGA[A]((TwoPSet(start, end), HashMap(start -> end), HashMap(start -> -1, end -> 0)))
}