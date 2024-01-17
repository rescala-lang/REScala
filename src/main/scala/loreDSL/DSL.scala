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
import lore.AST.*
import lore.DSL.*

class DSL extends StandardPlugin:
  val name: String = "DSL"
  val description: String = "Constructs a LoRe AST from the given Scala AST"

  override def init(options: List[String]): List[PluginPhase] =
    (new DSLPhase) :: Nil
end DSL

class DSLPhase extends PluginPhase:
  import tpd.*
  val phaseName: String = "DSL"
  var loreTerms: List[Term] = List()

  override val runsAfter: Set[String] = Set(Pickler.name)
  override val runsBefore: Set[String] = Set(Staging.name)

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree =
    tree match
      case ValDef(name, tpt, rhs) if name.toString.equals("firstRealVariable") =>
        tpt match
          case AppliedTypeTree(typeName: Ident, typeParameters: List[Ident]) =>
            println(typeName)
            println(typeParameters.mkString(", "))
            loreTerms ++: List(TAbs(name.toString, SimpleType(typeName.toString, typeParameters.map(t => SimpleType(t.toString, List()))), TNum(39)))
//            val num: TNum = TNum(39)
          case _ =>
            ()

        println("--------------------")
        println(s"name: $name")
        println("----------------")
        println(s"tpt: $tpt")
        println(s"tpt.tpe: ${tpt.tpe}")
        println(s"tpt.typeOpt: ${tpt.typeOpt}")
        println(s"tpt.toList: ${tpt.toList}")
        println(s"tpt.denot: ${tpt.denot}")
        println("----------------")
        println(s"rhs: $rhs")

        report.warning("First variable", tree.sourcePos)
//      case ValDef(name, tpt, rhs) if tpt.denot.name.toString.equals("Source") =>
//        report.warning("Source definition detected", tree.sourcePos)
      case _ =>
        ()
      tree
  end transformValDef
end DSLPhase
