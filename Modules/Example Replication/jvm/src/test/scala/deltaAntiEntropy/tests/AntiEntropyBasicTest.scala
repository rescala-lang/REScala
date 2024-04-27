package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Named, Network}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.base.Uid.asId
import rdts.base.{Bottom, Lattice, Uid}
import rdts.datatypes.GrowOnlyList.Node.Elem
import rdts.datatypes.contextual.ReplicatedList
import rdts.datatypes.{GrowOnlyList, LastWriterWins}
import rdts.dotted.{Dotted, HasDots}
import rdts.time.{CausalTime, Dot}
import replication.JsoniterCodecs.given
import test.rdts.DataGenerator.RGAGen.{makeRGA, given}

import scala.collection.mutable

class AntiEntropyBasicTest extends munit.ScalaCheckSuite {

  implicit val IntCodec: JsonValueCodec[String] = JsonCodecMaker.make
  given HasDots[Int]                            = HasDots.noDots
  given HasDots[String]                         = HasDots.noDots

  test("basic") {

    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[ReplicatedList[String]]("a", network, mutable.Buffer())

    val aec = AntiEntropyContainer[ReplicatedList[String]](ae)

    aec.insert(using aec.replicaID)(0, "00")

    aec.update(using Uid.predefined("b"))(0, "UPD")

    assertEquals(aec.toList, List("UPD"))

    aec.insert(using aec.replicaID)(1, "100")

    assertEquals(aec.toList, List("UPD", "100"))

    val lots = List.tabulate(100)(_.toString)

    lots.foreach: elem =>
      aec.insert(using aec.replicaID)(0, elem)
    // aec.insertAll(using aec.replicaID)(0, lots)

    assertEquals(aec.toList, lots.reverse ::: List("UPD", "100"))

    aec.insert(using Uid.predefined("b"))(1, "b00")

    assertEquals(aec.read(1), Some("b00"))

  }

  test("basic grow only list") {

    val network = new Network(0, 0, 0)

    val ae = new AntiEntropy[GrowOnlyList[String]]("a", network, mutable.Buffer())

    val aec = AntiEntropyContainer(ae)

    aec.insertGL(0, "00")

    assertEquals(aec.toList, List("00"))

    aec.insertGL(1, "100")

    assertEquals(aec.toList, List("00", "100"), aec.state)

    val lots = List.tabulate(100)(_.toString)

    lots.foreach: elem =>
      aec.insertGL(0, elem)
    // aec.insertAll(using aec.replicaID)(0, lots)

    assertEquals(aec.toList, lots.reverse ::: List("00", "100"))

    aec.insertGL(1, "b00")

    assertEquals(aec.read(1), Some("b00"))

  }

  test("specific property example") {

    implicit val IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make

    val inserted: List[(Int, Int)]                       = List((0, 100))
    val removed: List[Int]                               = Nil
    val insertedAB: (List[(Int, Int)], List[(Int, Int)]) = (List((1, 4678)), Nil)
    val removedAB: (List[Int], List[Int])                = (Nil, List(0))
    val updatedAB: (List[(Int, Int)], List[(Int, Int)])  = (Nil, Nil)
    val network: Network = NetworkGenerator(0.4413085645880368, 0.382367536830158, 0.7030039323564119).make()

    val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

    val la0 = AntiEntropyContainer(aea)
    la0.applyDelta(Named(
      Uid.predefined(aea.replicaID),
      makeRGA(inserted, removed, Uid.predefined(aea.replicaID))
    ))
    network.startReliablePhase()
    AntiEntropy.sync(aea, aeb)
    network.endReliablePhase()
    val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

    val la1 = {
      val inserted = insertedAB._1.foldLeft(la0) {
        case (rga, (i, e)) => rga.insert(using rga.replicaID)(i, e)
      }

      val deleted = removedAB._1.foldLeft(inserted) {
        case (rga, i) => rga.delete(using rga.replicaID)(i)
      }

      updatedAB._1.foldLeft(deleted) {
        case (rga, (i, e)) => rga.update(using rga.replicaID)(i, e)
      }
    }

    val lb1 = {
      val inserted = insertedAB._2.foldLeft(lb0) {
        case (rga, (i, e)) => rga.insert(using rga.replicaID)(i, e)
      }

      val deleted = removedAB._2.foldLeft(inserted) {
        case (rga, i) => rga.delete(using rga.replicaID)(i)
      }

      updatedAB._2.foldLeft(deleted) {
        case (rga, (i, e)) => rga.update(using rga.replicaID)(i, e)
      }
    }

    val beforeA1: Dotted[ReplicatedList[Int]] = la1.state
    val beforeB1                              = lb1.state

    AntiEntropy.sync(aea, aeb)
    network.startReliablePhase()
    AntiEntropy.sync(aea, aeb)

    val la2 = la1.processReceivedDeltas()
    val lb2 = lb1.processReceivedDeltas()

    assertEquals(
      beforeA1.decomposed.reduceOption(Lattice.merge).getOrElse(Bottom.empty[Dotted[ReplicatedList[Int]]]),
      beforeA1
    )
    assertEquals(
      beforeB1.decomposed.reduceOption(Lattice.merge).getOrElse(Bottom.empty[Dotted[ReplicatedList[Int]]]),
      beforeB1
    )

    val directMergedState = beforeA1 merge beforeB1
    assertEquals(la2.state, directMergedState)
    assertEquals(lb2.state, directMergedState)

    assertEquals(
      la2.toList,
      lb2.toList,
      s"After synchronization messages were reliably exchanged all replicas should converge, but ${la2.toList} does not equal ${lb2.toList}\n  before A: $beforeA1\n  before B: $beforeB1\n  ${la2.state}\n  ${lb2.state}"
    )
  }

}
