package benchmarks.lattices

import org.openjdk.jmh.annotations.*
import rdts.base.Lattice

import java.util.concurrent.TimeUnit
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.util.Random

object MergeImpl {
  def mergeKeySet[K, V: Lattice]: Lattice[Map[K, V]] =
    (left, right) =>
      (left.keysIterator ++ right.keysIterator)
        .toSet[K].iterator
        .flatMap { key =>
          Lattice.merge(left.get(key), right.get(key)).map(key -> _)
        }.toMap

  def mergeHashMap[K, V: Lattice]: Lattice[Map[K, V]] = new Lattice[Map[K, V]] {
    def merge(left: Map[K, V], right: Map[K, V]): Map[K, V] =
      left.to(HashMap).merged(right.to(HashMap)) {
        case ((id, v1), (_, v2)) => (id, (Lattice.merge(v1, v2)))
      }
  }

  def mergeMutable[K, V: Lattice]: Lattice[Map[K, V]] = new Lattice[Map[K, V]] {
    def merge(left: Map[K, V], right: Map[K, V]): Map[K, V] = {
      val aggregate = left.to(mutable.HashMap)
      right.foreach { case (k, r) =>
        aggregate.updateWith(k) {
          case None    => Some(r)
          case Some(l) => Some(Lattice[V].merge(l, r))
        }
      }
      aggregate.toMap
    }
  }

  def mergeFold[K, V: Lattice]: Lattice[Map[K, V]] =
    (left, right) =>
      right.foldLeft(left) {
        case (m, (k, r)) =>
          m.updatedWith(k) {
            case Some(l) => Some(Lattice.merge(l, r))
            case None    => Some(r)
          }
      }

  implicit def IntLattice: Lattice[Int] = _ max _
}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Thread)
class MapMergeBenchmark {

  @Param(Array("1", "100", "1000"))
  var leftSize: Int = scala.compiletime.uninitialized

  @Param(Array("1"))
  var rightSize: Int = scala.compiletime.uninitialized

  var left: Map[Int, Int]  = scala.compiletime.uninitialized
  var right: Map[Int, Int] = scala.compiletime.uninitialized

  import MergeImpl.IntLattice

  val latticeSet: Lattice[Map[Int, Int]]     = MergeImpl.mergeKeySet
  val latticeHash: Lattice[Map[Int, Int]]    = MergeImpl.mergeHashMap
  val latticeMutable: Lattice[Map[Int, Int]] = MergeImpl.mergeMutable
  val latticeFold: Lattice[Map[Int, Int]]    = MergeImpl.mergeFold

  @Setup
  def setup(): Unit = {
    left = Range(0, leftSize).map(k => k -> Random.nextInt()).toMap
    // right only selects keys present in left
    right = Range(0, rightSize).map(k => Random.nextInt(leftSize) -> k).toMap
  }

  @Benchmark
  def setMerge() = latticeSet.merge(left, right)

  @Benchmark
  def hashMerge() = latticeHash.merge(left, right)

  @Benchmark
  def mutableMerge() = latticeMutable.merge(left, right)

  @Benchmark
  def foldMerge() = latticeFold.merge(left, right)

}
