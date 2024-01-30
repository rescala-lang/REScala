package loreDSL

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.{Pickler, Staging}

class DSL extends StandardPlugin:
  val name: String = "DSL"
  val description: String = "Constructs a LoRe AST from the given Scala AST"

  override def init(options: List[String]): List[PluginPhase] =
    (new DSLPhase) :: Nil
end DSL

class DSLPhase extends PluginPhase:
  import tpd.*
  val phaseName: String = "DSL"

  override val runsAfter: Set[String] = Set(Pickler.name)
  override val runsBefore: Set[String] = Set(Staging.name)

  var loreTerms: List[Term] = List()
  private val reactiveTypeNames: List[String] = List(
    "Source", "Derived", "Interaction",
    "InteractionWithTypes", "InteractionWithRequires",
    "InteractionWithRequiresAndModifies"
  )

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree =
    tree match
      // Catch any Source, Derived and Interaction definitions
      case ValDef(name, tpt, rhs) if reactiveTypeNames.contains(tpt.denot.name.toString) =>
        tpt match
          // Doesn't work for Interaction, because it gets inlined to InteractionWithTypes etc.
          // Structure of InteractionWithTypes also seems different, it doesn't have a denotation,
          // is using a generic TypeTree and then an AppliedType, instead of an AppliedTypeTree.
          // Unsure how to match that part for type name / type parameters.
          // Try testing: tpt.tpe.argTypes and copying over the AST file into this project instead of importing lore
          case AppliedTypeTree(typeName: Ident, typeParameters: List[Ident]) =>
            report.warning(s"${typeName.name.toString} definition detected", tree.sourcePos)
            println("-------------")
            println(s"typeName: $typeName")
            println(s"typeParameters: ${typeParameters.mkString(", ")}")
            println(s"argTypes: ${tpt.tpe.argTypes.mkString(", ")}")
            loreTerms = loreTerms :+ TAbs(name.toString, SimpleType(typeName.name.toString, typeParameters.map(t => SimpleType(t.name.toString, List()))), TNum(39))
            println("-------")
            println(s"loreTerms:\n${loreTerms.mkString("\n")}")
            println("-------")
          case _ =>
            ()
      case ValDef(name, tpt, rhs) if name.toString.equals("thirdRealVariable") =>
        println("----------")
        println(s"name: $name")
        println("----------------")
        println(s"tpt: $tpt")
        println(s"tpt.tpe: ${tpt.tpe}")
        println(s"tpt.tpe.argTypes: ${tpt.tpe.argTypes.mkString(", ")}")
        println("----------------")
        println(s"rhs: $rhs")

        report.warning("Third variable", tree.sourcePos)
      case _ =>
        ()
      tree
  end transformValDef
end DSLPhase
