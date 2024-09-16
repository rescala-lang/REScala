package ex2024bft

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Fork
import org.openjdk.jmh.annotations.Level
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.Param
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Warmup
import org.openjdk.jmh.infra.Blackhole
import rdts.base.{Bottom, Lattice, LocalUid}
import rdts.datatypes.GrowOnlyCounter
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
class BenchmarkState {
  @Param(Array("1", "2", "5", "10", "25", "50", "100", "250", "500", "1000", "2500", "5000", "10000"))
  var size = 0

  var gocList: List[GrowOnlyCounter]       = null
  var gocLattice: Lattice[GrowOnlyCounter] = null

  var bftGOCList: List[BFT[GrowOnlyCounter]]       = null
  var bftGOCLattice: Lattice[BFT[GrowOnlyCounter]] = null

  var repLL: List[Dotted[ReplicatedList[Int]]]           = null
  var repLLLattice: Lattice[Dotted[ReplicatedList[Int]]] = null

  var bftRepLL: List[BFT[Dotted[ReplicatedList[Int]]]]           = null
  var bftRepLLLattice: Lattice[BFT[Dotted[ReplicatedList[Int]]]] = null

  @Setup(Level.Trial)
  def setup(): Unit = {
    gocList = BFTBenchmark.generateGOCList(size)
    gocLattice = BFTBenchmark.gocLattice

    bftGOCList = BFTBenchmark.generateGOCBFTList(size)
    bftGOCLattice = BFTBenchmark.bftGOCLattice

    repLL = BFTBenchmark.generateListDeltaList(size)
    repLLLattice = BFTBenchmark.dottedRepListIntLattice

    bftRepLL = BFTBenchmark.generateBFTListDeltaList(size)
    bftRepLLLattice = BFTBenchmark.latticeBFTListDeltaList
  }

}

@Fork(value = 1, warmups = 0)
@Warmup(iterations = 2)
@Measurement(iterations = 2)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class BFTBenchmark {

  @Benchmark
  def baselineGOC(blackhole: Blackhole, state: BenchmarkState): Unit = {
    blackhole.consume(state.gocList.reduce((left, right) => state.gocLattice.merge(left, right)))
  }

  @Benchmark
  def bftGOC(blackhole: Blackhole, state: BenchmarkState): Unit = {
    blackhole.consume(state.bftGOCList.reduce((left, right) => state.bftGOCLattice.merge(left, right)).value)
  }

  @Benchmark
  def baselineRepList(blackhole: Blackhole, state: BenchmarkState): Unit = {
    blackhole.consume(state.repLL.reduce((left, right) => state.repLLLattice.merge(left, right)))
  }

  @Benchmark
  def bftRepList(blackhole: Blackhole, state: BenchmarkState): Unit = {
    blackhole.consume(state.bftRepLL.reduce((left, right) => state.bftRepLLLattice.merge(left, right)).value)
  }

}

object BFTBenchmark {
  def generateGOCList(size: Int): List[GrowOnlyCounter] = {
    val id1 = LocalUid.gen()

    var goc = summon[Bottom[GrowOnlyCounter]].empty

    var list = List.empty[GrowOnlyCounter]

    list +:= goc

    for _ <- 0 to size do {
      goc = goc.inc()(using id1)
      list +:= goc
    }

    list
  }

  def gocBottom: Bottom[GrowOnlyCounter]   = summon[Bottom[GrowOnlyCounter]]
  def gocLattice: Lattice[GrowOnlyCounter] = summon[Lattice[GrowOnlyCounter]]

  def generateGOCBFTList(size: Int): List[BFT[GrowOnlyCounter]] = {
    val id1 = LocalUid.gen()

    var bft = BFT(summon[Bottom[GrowOnlyCounter]].empty)(using byteableGOC)

    var list = List.empty[BFT[GrowOnlyCounter]]

    list +:= bft

    for _ <- 0 to size do {
      bft = bft.update(_.inc()(using id1))(using byteableGOC, gocLattice, gocBottom)
      list +:= bft
    }

    list
  }

  def byteableGOC: Byteable[GrowOnlyCounter]       = it => it.inner.toString.getBytes
  def bftGOCLattice: Lattice[BFT[GrowOnlyCounter]] = BFT.lattice(using gocLattice)(using byteableGOC)

  def generateListDeltaList(size: Int): List[Dotted[ReplicatedList[Int]]] = {
    val id1 = LocalUid.gen()

    var repList = summon[Bottom[Dotted[ReplicatedList[Int]]]].empty

    var list = List.empty[Dotted[ReplicatedList[Int]]]

    list +:= repList

    for i <- 0 to size do {
      repList = repList.mod(_.insert(using id1)(0, i))
      list +:= repList
    }

    list
  }

  def dottedRepListIntLattice: Lattice[Dotted[ReplicatedList[Int]]] = summon[Lattice[Dotted[ReplicatedList[Int]]]]

  def generateBFTListDeltaList(size: Int): List[BFT[Dotted[ReplicatedList[Int]]]] = {
    val id1 = LocalUid.gen()

    var bft = BFT(bottomListDeltaList.empty)(using byteableListDeltaList)

    var list = List.empty[BFT[Dotted[ReplicatedList[Int]]]]

    list +:= bft

    for i <- 0 to size do {
      bft = bft.update(_.mod(_.insert(using id1)(0, i)))(using
        byteableListDeltaList,
        dottedRepListIntLattice,
        bottomListDeltaList
      )
      list +:= bft
    }

    list
  }

  def bottomListDeltaList: Bottom[Dotted[ReplicatedList[Int]]]     = summon
  def byteableListDeltaList: Byteable[Dotted[ReplicatedList[Int]]] = Byteable.toStringBased
  def latticeBFTListDeltaList: Lattice[BFT[Dotted[ReplicatedList[Int]]]] =
    BFT.lattice(using dottedRepListIntLattice)(using byteableListDeltaList)

}
