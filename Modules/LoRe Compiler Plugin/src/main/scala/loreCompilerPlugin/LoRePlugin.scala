package loreCompilerPlugin

import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.ast.Trees.{Apply, Block, Select, ValDef}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{Inlining, Pickler}
import dotty.tools.dotc.util.SourceFile
import java.io.File // For getting URIs and the system-independent path separator
import lore.ast.{SimpleType, TAbs, TDerived, TSource, Term, TupleType}
import loreCompilerPlugin.codegen.LoReGen.{buildLoreRhsTerm, buildLoreTypeNode}
import loreCompilerPlugin.codegen.DafnyGen
import loreCompilerPlugin.lsp.DafnyLSPClient
import loreCompilerPlugin.lsp.LSPDataTypes.{LSPNotification, NamedVerifiable, SymbolStatusNotification, VerificationStatus}
import ujson.Obj

import scala.annotation.nowarn

class LoRePlugin extends StandardPlugin {
  val name: String        = "LoRe Compiler Plugin"
  val description: String = "Verifies Scala-embedded LoRe code through Dafny"

  @nowarn // which variant to override depends on the scala version, use the old one until 3.5 is more stable
  override def init(options: List[String]): List[PluginPhase] = List(new LoRePhase)
}

object LoRePhase {
  val name: String        = "LoRe"
  val description: String = "verifies LoRe source code through compilation to Dafny"
}

class LoRePhase extends PluginPhase {
  val phaseName: String                = LoRePhase.name
  override val description: String     = LoRePhase.description
  override val runsAfter: Set[String]  = Set(Pickler.name)
  override val runsBefore: Set[String] = Set(Inlining.name)

  private var loreTerms: Map[(SourceFile, Symbol), List[Term]] = Map()

  override def transformValDef(tree: tpd.ValDef)(using ctx: Context): tpd.Tree = {
    var newLoreTerm: Option[Term] = None // Placeholder, value is defined in below individual cases to avoid code dupe

    tree match
      case ValDef(name, tpt, rhs) =>
        rhs match
          case tpd.EmptyTree => () // Function parameter and Part 1 of object/package definitions, these are ignored
          case Apply(Select(_, n), _) if n.toString.equals("<init>") => () // Part 2 of Object and package definitions
          case _ => // Other definitions, these are the ones we care about
            val loreTypeNode = buildLoreTypeNode(tpt.tpe, tpt.sourcePos)
            // Several notes to make here regarding handling the RHS of reactives for future reference:
            // * There's an Apply around the whole RHS whose significance I'm not exactly sure of.
            //   Maybe it's related to a call for Inlining or such, as this plugin runs before that phase
            //   and the expressions being handled here use types that get inlined in REScala.
            // * Because the RHS is wrapped in a call to the respective reactive type, within that unknown Apply layer,
            //   there's one layer of an Apply call to the REScala type wrapping the RHS we want, and the
            //   actual RHS tree we want is inside the second Apply parameter list (i.e. real RHS is 2 Apply layers deep).
            // * As the Derived type uses curly brackets in definitions on top, it additionally wraps its RHS in a Block type.
            // * The Source and Derived parameter lists always have length 1, those of Interactions always have length 2.
            // * Typechecking for whether all of this is correct is already done by the Scala type-checker before this phase,
            //   so we can assume everything we see here is of suitable types instead of doing any further checks.
            loreTypeNode match
              case SimpleType(typeName, typeArgs) =>
                println(s"Detected $typeName definition with name \"$name\"")
                rhs match
                  case tpd.EmptyTree => () // Ignore func args (ArgT) outside of arrow functions for now
                  case Apply(Apply(_, List(properRhs)), _)
                      if typeName == "Var" => // E.g. "foo: Source[bar] = Source(baz)"
                    newLoreTerm = Some(TAbs( // foo: Source[Bar] = Source(baz)
                      name.toString, // foo
                      loreTypeNode, // Source[Bar]
                      TSource(buildLoreRhsTerm(properRhs, 1)) // Source(baz)
                    ))
                  case Apply(Apply(_, List(Block(_, properRhs))), _)
                      if typeName == "Signal" => // E.g. "foo: Derived[bar] = Derived { baz } "
                    newLoreTerm = Some(TAbs( // foo: Derived[Bar] = Derived { baz }
                      name.toString, // foo
                      loreTypeNode, // Derived[Bar]
                      TDerived(buildLoreRhsTerm(properRhs, 1)) // Derived { baz }
                    ))
                  case _ => // Interactions (UnboundInteraction, ...) and any non-reactive RHS (Int, String, Bool, ...)
                    newLoreTerm = Some(TAbs( // foo: Bar = baz
                      name.toString, // foo (any valid Scala identifier)
                      loreTypeNode, // Bar
                      buildLoreRhsTerm(rhs, 1) // baz (e.g. 0, 1 + 2, "test", true, 2 > 1, bar as a reference, etc)
                    ))
              case TupleType(_) => // TODO tuple types?
                println(s"Detected tuple type, these are currently unsupported. Tree:\n$tree")
                report.error("LoRe Tuple Types are not currently supported", tree.sourcePos)

    // Add the newly generated LoRe term to the respective source file's term list (if one was generated)
    newLoreTerm match
      case None =>
        tree // No term created, return the original tree to further compiler phases
      case Some(loreTerm) =>
        // Find term list for this source file and append new term to it
        // Alternatively, make new term list for this file if it doesn't exist
        val fileTermList: Option[List[Term]] = loreTerms.get((tree.source, ctx.owner))
        fileTermList match
          case Some(list) =>
            val newList: List[Term] = list :+ loreTerm
            loreTerms = loreTerms.updated((tree.source, ctx.owner), newList)
          case None =>
            println(s"Adding new term list to Map for ${ctx.owner.toString} in ${tree.source.name}")
            val newList: List[Term] = List(loreTerm)
            loreTerms = loreTerms.updated((tree.source, ctx.owner), newList)
        tree // Return the original tree to further compiler phases
  }

  override def runOn(units: List[CompilationUnit])(using ctx: Context): List[CompilationUnit] = {
    // First, run the runOn method for regular Scala compilation (do not remove this or this phase breaks).
    // This will cause the compiler to call above-defined methods which will generate the LoRe AST nodes.
    val result = super.runOn(units)

    println("LoRe AST node generation was run for all compilation units.")
    println(s"Processed ${loreTerms.size} compilation units with ${loreTerms.map(_._2.size).mkString(",")} terms each.")

    // Initialize LSP client that will be used for verification after codegen
    val lspClient: DafnyLSPClient = new DafnyLSPClient()

    // Get the root path of the project these compilation units are from.
    // Basically, cut off anything that comes after "src/main/scala".
    // Have to do some messing around with escaping slashes and URIs here.
    val unitPath: String    = units.head.source.path
    val rootPattern: String = List("src", "main", "scala").mkString(File.separator) // Windows/Unix separators
    val rootPatternEscaped: String = rootPattern.replace("\\", "\\\\") // Gotta escape since split takes regex
    // Take the first half of the split, then add the split off separator back on and get the path as an URI string
    val folderPath: String = File(unitPath.split(rootPatternEscaped).head.concat(rootPattern)).toURI.toString
    lspClient.initializeLSP(folderPath)

    var counter: Int = 0

    // Iterate through all term lists and generate Dafny code for them + verify it
    for termList <- loreTerms do {
      println(s"Now processing LoRe AST parts for compilation unit: ${termList._1}.")
      // Turn the filepath into an URI and then sneakily change the file extension the LSP gets to see
      val filePath: String = File(termList._1._1.path).toURI.toString.replace(".scala", ".dfy")

      val test: String = DafnyGen.generate(termList._2.head)

      counter += 1
      // Generate dafny code from term list
      // todo: this is dummy code, normally output by the to-be-implemented dafny generator
      val dafnyCode: String =
        s"""method Test(x: int) returns (y: int)
           |  ensures {:error "Error on LoRe ln X, col Y"} y == ${if counter == 1 then "x" else counter}
           |  {
           |    y := x;
           |  }
           |
           |method Main()
           |{
           |  var a: int := Test(0);
           |  print a;
           |}""".stripMargin

      // Send the generated code "file" to the language server to verify
      val didOpenMessage: String = DafnyLSPClient.constructLSPMessage("textDocument/didOpen")(
        (
          "textDocument",
          Obj(
            "uri"        -> filePath,
            "languageId" -> "dafny",
            "version"    -> 1,
            "text"       -> dafnyCode
          )
        ),
      )
      lspClient.sendMessage(didOpenMessage)

      // Wait for verification results and then filter out any verification errors that occurred
      val (verificationResult: SymbolStatusNotification, diagnosticsNotification: Option[LSPNotification]) =
        lspClient.waitForVerificationResult()

      val erroneousVerifiables: List[NamedVerifiable] =
        verificationResult.params.namedVerifiables.filter(nv => nv.status == VerificationStatus.Error)

      // Process potential verification errors
      if erroneousVerifiables.isEmpty then {
        println("No unverifiable claims could be found in the program.")
        // This report is a debug log, remove this later and make it silent
        report.log("All claims were verified successfully.")
      } else {
        println("Some claims in the program could not be verified.")
        // Add details to this later, just simple debug for now
        report.error("Some claims could not be verified.")
      }
    }

    // Always return result of default runOn method for regular Scala compilation, as we do not modify it.
    result
  }
}
