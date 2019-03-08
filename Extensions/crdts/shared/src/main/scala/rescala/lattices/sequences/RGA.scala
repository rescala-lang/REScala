package rescala.lattices.sequences

import rescala.lattices.Lattice
import rescala.lattices.sets.{TwoPSet}

import scala.collection.immutable.HashMap

/**
  * Replicated Growable Array
  *
  * @tparam A The type of the elements stored in this array
  */
case class RGA[A](vertices: TwoPSet[Vertex],
                  edges: Map[Vertex, Vertex],
                  values: Map[Vertex, A])
  extends CRDTSequence[A, TwoPSet[Vertex], RGA[A]] {

  def remove(v: Seq[Vertex]): RGA[A] = copy(vertices = vertices.remove(v))
  def filter(keep: A => Boolean): RGA[A] = {
    val removed = values.collect { case (k, v) if !keep(v) => k }
    println(s"removing $removed")
    remove(removed.toList)
  }

  override def copySub(vertices: TwoPSet[Vertex],
                       edges: Map[Vertex, Vertex],
                       values: Map[Vertex, A]): RGA[A] = copy(vertices, edges, values)
}

/**
  * Replicated Grow Only Array - A modified RGA version using grow only sets for vertices
  *
  * @param payload The payload consist of a Grow-Only Set which stores the vertices and one HashMap which stores the
  *                edges between vertices.
  * @tparam A The type of the elements stored in this array.
  */
case class RGOA[A](vertices: Set[Vertex],
                   edges: Map[Vertex, Vertex],
                   values: Map[Vertex, A])
  extends CRDTSequence[A, Set[Vertex], RGOA[A]] {
  override def copySub(vertices: Set[Vertex],
                       edges: Map[Vertex, Vertex],
                       values: Map[Vertex, A]): RGOA[A] = copy(vertices, edges, values)

}

object RGOA {

  def empty[A]: RGOA[A] = new RGOA[A](Set[Vertex](), HashMap[Vertex, Vertex](Vertex.start -> Vertex.end), Map())


  /** Allows the creation of new CRDTs by passing an initial value.
    *
    * @param value the value
    * @return new CRDT instance representing the value
    */
  def apply[A](value: Seq[A]): RGOA[A] = {
    value.reverse.foldLeft(empty[A]) {
      case (r: RGOA[A], a) => r.addRight(Vertex.start, a)
    }
  }


  implicit def crdt[A]: Lattice[RGOA[A]] = new Lattice[RGOA[A]] {
    override def merge(left: RGOA[A], right: RGOA[A]): RGOA[A] = {
      println(s"Merging $right into $left")

      val newVertices = right.vertexIterator.toList.filter(!left.edges.contains(_))
      println(s"found new vertices in right: $newVertices")

      // build map of old insertion positions of the new vertices
      val oldPositions = right.edges.foldLeft(Map[Vertex, Vertex]()) {
        case (map, (l, r)) => if (newVertices.contains(r)) map + (r -> l) else map
      }

      // update edges by inserting vertices at the right positions
      val partialnew = newVertices.foldLeft(left) {
        case (merged, v) =>
          println(s"inserting $v at position ${oldPositions(v)}")
          merged.addRight(oldPositions(v), v, right.values(v))
      }
      partialnew.copy(vertices = Lattice.merge(left.vertices, right.vertices))
    }

  }
}




object RGA {




  def apply[A](values: Seq[A]): RGA[A] = {
    values.reverseIterator.foldLeft(empty[A]) {
      case (r, a) => r.addRight(Vertex.start, a)
    }
  }

  def empty[A]: RGA[A] = new RGA[A](TwoPSet[Vertex](), Map(Vertex.start -> Vertex.end), Map())


  implicit def RGA2CRDTInstance[A]: Lattice[RGA[A]] =
    new Lattice[RGA[A]] {
      override def merge(left: RGA[A], right: RGA[A]): RGA[A] = {
        val newVertices = right.vertexIterator.toList.filter(!left.edges.contains(_))

        // build map of old insertion positions of the new vertices
        val oldPositions = right.edges.foldLeft(Map(): Map[Vertex, Vertex]) {
          case (m, (u, v)) => if (newVertices.contains(v)) m + (v -> u) else m
        }

        val partialnew =  newVertices.foldLeft(left) { case (merged: RGA[A], v: Vertex) =>
            merged.addRight(oldPositions(v), v, right.values(v))
        }

        partialnew.copy(vertices = TwoPSet.instance.merge(left.vertices, right.vertices))
      }
    }
}

