package rescala.crdts.statecrdts
package sequences

import rescala.crdts.statecrdts.sets.StateCRDTSet

import scala.collection.immutable.HashMap

trait StateCRDTSequence[A] {
  def vertices: StateCRDTSet[Vertex[Any]]

  def edges: HashMap[Vertex[Any], Vertex[Any]]

  def contains[A1 >: A](v: Vertex[A1]): Boolean = v match {
    case Vertex.start => true
    case Vertex.end => true
    case v: Vertex[A] => vertices.contains(v)
  }

  def containsValue(a: A): Boolean = vertices.value.map(_.value).contains(a)

  def before[A1 >: A](u: Vertex[A1], v: Vertex[A1]): Boolean
}
