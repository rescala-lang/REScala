package rescala.crdts.statecrdts.sequences

import rescala.crdts.statecrdts.sets.StateCRDTSet

import scala.collection.immutable.HashMap

trait StateCRDTSequence[A] {
  def vertices: StateCRDTSet[ValueVertex[A]]

  def edges: HashMap[Vertex, Vertex]

  def contains(v: Vertex): Boolean = v match {
    case `startVertex` => true
    case `endVertex` => true
    case v: ValueVertex[A] => vertices.contains(v)
  }

  def containsValue(a: A): Boolean = vertices.value.map(_.value).contains(a)

  def before(u: Vertex, v: Vertex): Boolean
}
