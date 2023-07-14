package test.kofre

import kofre.base.Time
import kofre.datatypes.contextual.LastWriterWins
import kofre.dotted.Dotted
import kofre.syntax.ReplicaId
import kofre.time.Dot
import kofre.time.Dots

class RemoveDotsManualTests extends munit.ScalaCheckSuite {

  object OptionManualTestsUtils {
    val r1: ReplicaId = ReplicaId.gen()
    val r2: ReplicaId = ReplicaId.gen()
    assert(r1 != r2)

    val dot_1_0: Dot = Dot(r1.uid, 0)
    val dot_1_1: Dot = dot_1_0.advance
    val dot_1_2: Dot = dot_1_1.advance

    val dot_2_0: Dot = Dot(r2.uid, 0)
    val dot_2_1: Dot = dot_2_0.advance
    val dot_2_2: Dot = dot_2_1.advance

    val c_0: Dots = Dots.empty.add(dot_1_0).add(dot_2_0)
    val c_1: Dots = Dots.empty.add(dot_1_1).add(dot_2_1)
    val c_2: Dots = Dots.empty.add(dot_1_2).add(dot_2_2)
  }
  import OptionManualTestsUtils._

  test("Dotted[LastWriterWins[Option[Int]]] removeDots") {
    val empty: Dotted[LastWriterWins[Option[Int]]] = Dotted(LastWriterWins[Option[Int]](dot_1_0, Time.current(), None), c_0)

    // remove current dots deletes
    val empty_r_0_0: Option[LastWriterWins[Option[Int]]] = empty.data.removeDots(c_0)
    assertEquals(empty_r_0_0, None)


    val val_1: Dotted[LastWriterWins[Option[Int]]] = empty.write(using r1)(Some(1).asInstanceOf[Option[Int]])
    assertEquals(val_1.read, Some(1))

    // remove old dots keeps new value
    val empty_r_1_0: Option[LastWriterWins[Option[Int]]] = val_1.data.removeDots(c_0)
    assertEquals(empty_r_1_0, Some(val_1.data))

    // remove current dots deletes
    val empty_r_1_1: Option[LastWriterWins[Option[Int]]] = val_1.data.removeDots(c_1)
    assertEquals(empty_r_1_1, None)

    // remove future dots deletes
    val empty_r_1_2: Option[LastWriterWins[Option[Int]]] = val_1.data.removeDots(c_2)
    assertEquals(empty_r_1_2, Some(val_1.data))


    val val_2: Dotted[LastWriterWins[Option[Int]]] = val_1.write(using r1)(Some(2).asInstanceOf[Option[Int]])
    assertEquals(val_2.read, Some(2))

    // remove old dots keeps new value
    val empty_r_2_0: Option[LastWriterWins[Option[Int]]] = val_2.data.removeDots(c_0)
    assertEquals(empty_r_2_0, Some(val_2.data))

    // remove old dots keeps new value
    val empty_r_2_1: Option[LastWriterWins[Option[Int]]] = val_2.data.removeDots(c_1)
    assertEquals(empty_r_2_1, Some(val_2.data))

    // remove current dots deletes
    val empty_r_2_2: Option[LastWriterWins[Option[Int]]] = val_2.data.removeDots(c_2)
    assertEquals(empty_r_2_2, None)
  }

}
