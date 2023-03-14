package test.kofre

import kofre.base.{Bottom, Lattice, Uid}
import kofre.datatypes.{EnableWinsFlag, LastWriterWins, PosNegCounter}
import kofre.dotted.{Dotted, DottedLattice}
import kofre.syntax.{DeltaBuffer, OpsSyntaxHelper, ReplicaId}
import kofre.time.{Dot, Dots}
import test.kofre.Project.ProjectSyntax

case class Project(
    _name: LastWriterWins[Option[String]],
    _max_hours: PosNegCounter,
    _account_name: LastWriterWins[Option[String]],
) derives DottedLattice

object Project {
  val empty: Dotted[Project] =
    val dot = Dot(Uid.predefined(""), 0)

    Dotted(
      Project(
        LastWriterWins.empty(dot),
        PosNegCounter.zero,
        LastWriterWins.empty(dot.advance)
      ),
      Dots.single(dot).add(dot.advance)
    )

  implicit class ProjectSyntax[C](container: C)
      extends OpsSyntaxHelper[C, Project](container) {

    // As a note, these methods are added as kind of extension methods to `Project` and RDT containers of project.
    // Its generally a good idea to not have them clash with the methods or values defined on `Project`,
    // specifically, I renamed the members of `Project` to start with an _ in this case.
    def name(using PermQuery): String         = current._name.read.getOrElse("no name")
    def max_hours(using PermQuery): Int       = current._max_hours.value
    def account_name(using PermQuery): String = current._account_name.read.getOrElse("no account")

    // this or something similar should maybe be added to OpsSyntaxHelper at some point.
    // Basically, the problem is that we want to modify the inner name, but within the current context,
    // which in turn also produces a new context.
    // To do so, we basically use some projection `p` to get whatever part of project we want to have,
    // then we re-wrap that into a DottedName which makes all of the syntax of that projected value available.
    // We pass the re-wrapped value to a mapping function `f` that can modify it as any other thing that has a contex and name.
    // We then return only the `Dotted` (dropping the name) as a convenience – the name never changes.
    private def focus[B, C](p: Project => B)(f: Dotted[B] => Dotted[C])(using
        PermCausalMutate,
    ): Dotted[C] = {
      f(context.wrap(p(current)))
    }

    def set_name(using ReplicaId, PermCausalMutate)(newName: String): C = {
      val updatedNameRegister: Dotted[LastWriterWins[Option[String]]] = focus(_._name)(_.write(Option(newName)))

      val projectDelta = empty.map(_.copy(_name = updatedNameRegister.data))
      // Every syntax function that uses a CausalMutationP always returns both an updated context and an updated value.
      // The updated context was produces by the `write` of the CausalLastWriterWins, while the value is the full project.
      // Note, if there are multiple different things written within the same mutation,
      // it is important to pass the correct context along
      // – this is currently not handled by the focus method above
      // (the `context` inside there always just returns the initial context).
      Dotted(
        projectDelta.data,
        updatedNameRegister.context,
      ).mutator
      // the above is the same as
      // updatedNameRegister.map(_ => projectDelta).mutator
    }
  }
}

class ProjectRDTTest extends munit.FunSuite {

  given fixedId: ReplicaId = kofre.base.Uid.predefined("replica id")

  test("basic interaction") {

    // The thing about the name and dots is that you always want to add them at the outermost layer,
    // such that the state is shared for everything inside your datatype.
    // specifically, the two CausalLastWriterWinsRegisters inside of project will now use the same context.
    val p = Project.empty

    // The whole point of the OpsSyntax above (and all its current unfortunate complications)
    // is to enable you to call the operations on different wrapper classes, such as this DottedName wrapper.
    // In particular, the way this is supposed to work at some point is that REScala provides a single global
    // name and a single global context (the thing in Dotted) that is used by all RDTs.
    // This is not yet implemented on the REScala side, but you may want to consider to implement something like that yourself.
    val deltaRes: Dotted[Project] = p.set_name("some project")
    // Note that the way this operation (and most other operations in REScala/kofre) is to return only a delta
    // when you want to have the full result object, you first have to merge the result with the prior state.
    // DottedName by itself is not a lattice (as it is impossible to merge things with different names),
    // so we merge the results dropping the name.
    // This changes the result type to just
    val newProject: Dotted[Project] = p.merge(deltaRes)

    // I did not actually run this, but that should work™ :-)
    assert(newProject.name == "some project")
    assert(newProject.map(_._name).read == Some("some project"))
  }

  test("pos neg delta buffer") {
    val deltaBufferRdt    = DeltaBuffer(PosNegCounter.zero)
    val newDeltaBufferRdt = deltaBufferRdt.posNegCounter.add(1)
    assertEquals(newDeltaBufferRdt.value, 1)
  }

  test("LWW delta buffer") {
    val deltaBufferRdt    = DeltaBuffer(Dotted(LastWriterWins.now[String](Dot(Uid.predefined(""), 0), "")))
    val newDeltaBufferRdt = deltaBufferRdt.write("test")
    assertEquals(newDeltaBufferRdt.read, "test")
  }

  test("Project delta buffer") {
    val deltaBufferRdt    = DeltaBuffer(Project.empty)
    val newDeltaBufferRdt = deltaBufferRdt.set_name("some project")
    // assertEquals(newDeltaBufferRdt.deltaBuffer, Nil)
    assertEquals(newDeltaBufferRdt.name, "some project")
  }

  test("delta filtering") {
    val init: Dotted[Project] = Project.empty
    val delta                 = init.set_name("some project")

    val dlat = Lattice[Dotted[Project]]

    val decomposed = dlat.decomposed(delta)
    val diff       = dlat.diff(init, delta)

    val recomposed = decomposed.reduce(_ merge _)

    assertEquals(delta <= init, false)
    assertEquals(delta <= init, false)
    assertEquals(recomposed, delta)
    assertEquals(diff, Some(delta))

  }

}
