package api

import clangast.WithContext
import clangast.types.CType
import compiler.MacroCompiler

case class Snapshot[B](e: Event[?], input: Fold[B], cType: WithContext[CType]) extends Event[B] {
  override def inputs: List[ReSource] = List(e, input)

  override val baseName: String = "snapshot"
}

object Snapshot {
  class SnapshotFactory[B](e: Event[?]) {
    inline def apply[C <: MacroCompiler](input: Fold[B])(using mc: C): Snapshot[B] =
      Snapshot(
        e,
        input,
        mc.compileType[Option[B]]
      )
  }
}

extension (e: Event[?])
  def snapshot[B]: Snapshot.SnapshotFactory[B] = new Snapshot.SnapshotFactory(e)
