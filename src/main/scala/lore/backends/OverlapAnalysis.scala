package lore.backends
import lore.AST._

object OverlapAnalysis:

  def overlappingInvariants(interaction: TInteraction)(using
      ctx: CompilationContext
  ): Set[TInvariant] =
    val subgraph = reaches(interaction)

    return ctx.invariants
      .map(i => (i, uses(i)))
      .filter((interaction, reactives) =>
        !(subgraph intersect reactives).isEmpty
      )
      .map((interaction, reactives) => interaction)
      .toSet

  /** Consumes an interaction and a compilation context and returns all
    * reactives that are affected by this interaction.
    * @param interaction
    * @param ctx
    * @return
    *   set of ids of the affected reactives
    */
  def reaches(interaction: TInteraction)(using
      ctx: CompilationContext
  ): Set[ID] =
    val graph = ctx.graph.view.mapValues((r, t) => r).toMap
    val sources = ctx.sources.filter { case (name, (source, _type)) =>
      interaction.modifies.contains(name)
    }.keySet
    val dependencies: Map[String, Set[ID]] =
      ctx.graph.keys.map(name => (name, getSubgraph(name, graph))).toMap

    def children(r: ID): Set[ID] =
      dependencies
        // filter reactives where r is in the dependencies
        .filter((r2, deps) => deps.contains(r))
        .view
        .keys
        .toSet

    return interaction.modifies.flatMap(children).toSet
