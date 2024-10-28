import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Arbitrary, Gen, Prop}
import rdts.base.{Lattice, LocalUid}
import rdts.datatypes.experiments.protocols.{Consensus, Participants}

import scala.util.Try

class ConsensusPropertySpec[A: Arbitrary, C[_]: Consensus](
    logging: Boolean = false,
    minDevices: Int,
    maxDevices: Int,
    writeFreq: Int,
    upkeepFreq: Int,
    mergeFreq: Int
)(using Lattice[C[A]]) extends CommandsARDTs[C[A]] {

  override def genInitialState: Gen[State] =
    for
      numDevices <- Gen.choose(minDevices, maxDevices)
      ids = Range(0, numDevices).map(_ => LocalUid.gen()).toList
    yield ids.map(id => (id, Consensus[C].empty)).toMap

  // generators
  override def genCommand(state: State): Gen[Command] =
    Gen.frequency(
      (writeFreq, genWrite(state)),
      (upkeepFreq, genUpkeep(state)),
      (mergeFreq, genMerge(state))
    )

  def genWrite(state: State): Gen[Write] =
    for
      id    <- genId(state)
      value <- arbitrary[A]
    yield Write(id, value)

  def genUpkeep(state: State) =
    genId(state).map(Upkeep(_))

  // commands that change state
  class Write(writer: LocalUid, value: A) extends ACommand(writer):
    def nextLocalState(states: Map[LocalUid, C[A]]) =

      given Participants = Participants(states.keySet.map(_.uid))
      Lattice[C[A]].merge(states(writer), states(writer).write(value)(using writer))

    override def postCondition(state: Map[LocalUid, C[A]], result: Try[Map[LocalUid, C[A]]]) =
      given Participants = Participants(state.keySet.map(_.uid))
      (state(writer).members == result.get(writer).members)
      :| s"Members do not change during writes.\nBefore: ${state(writer)}\nAfter:${result.get(writer)}"

  class Upkeep(id: LocalUid) extends ACommand(id):
    def nextLocalState(states: Map[LocalUid, C[A]]) =
      given Participants = Participants(states.keySet.map(_.uid))
      Lattice[C[A]].merge(states(id), states(id).upkeep()(using id))

    override def postCondition(state: Map[LocalUid, C[A]], result: Try[Map[LocalUid, C[A]]]): Prop =
      given Participants = Participants(state.keySet.map(_.uid))
      val res            = result.get
      val resValue       = res(id).read

      if logging && resValue.nonEmpty then println(s"accepted value: ${resValue.get}")

      // if two devices read a value it has to be the same
      Prop.forAll(genId(res)) {
        index =>
          (resValue, res(index).read) match
            case (Some(v1), Some(v2)) =>
              (v1 == v2) :| s"if two devices read a value, it has to be the same, got $v1, $v2" &&
              res(index).members.nonEmpty :| s"members are never empty" &&
              (state(index).members == res(index).members) :| s"upkeep does not change members"
            case _ => Prop(true)
      }
}
