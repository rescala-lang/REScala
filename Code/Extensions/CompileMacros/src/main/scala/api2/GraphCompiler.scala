package api2

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.{CAssignmentExpr, COrExpr}
import clangast.stmt.*
import clangast.stubs.StdBoolH
import clangast.types.*
import compiler.CompilerCascade
import compiler.context.TranslationContext
import compiler.base.*
import compiler.base.CompileDataStructure.{deepCopy, release, retain}
import compiler.ext.*

import java.io.{File, FileWriter}
import scala.collection.mutable.ArrayBuffer
import scala.quoted.*

class GraphCompiler(using Quotes)(reactives: List[CompiledReactive], appName: String)(using ctx: TranslationContext, cascade: CompilerCascade) {
  import quotes.reflect.*

  private val sources: List[CompiledReactive] = reactives.filter(_.isSource)
  private val signals: List[CompiledSignal] = reactives.collect { case s: CompiledSignal => s }
  private val events: List[CompiledEvent] = reactives.collect { case e: CompiledEvent => e }

  private val nameToReactive: Map[String, CompiledReactive] = reactives.map(r => r.name -> r).toMap
  private val reactiveInputs: Map[CompiledReactive, List[CompiledReactive]] =
    reactives.map(r => r -> r.inputs.map(nameToReactive)).toMap
  private val dataflow: Map[CompiledReactive, Set[CompiledReactive]] =
    reactives.foldLeft(Map.empty[CompiledReactive, Set[CompiledReactive]]) { (acc, r) =>
      addValueToAllKeys(acc, reactiveInputs(r), r)
    }

  private val topological: List[CompiledReactive] = toposort(sources).reverse
  private val sourcesTopological: List[CompiledReactive] = topological.filter(_.isSource)

  private def addValueToAllKeys[K, V](originalMap: Map[K, Set[V]], keys: List[K], value: V): Map[K, Set[V]] = {
    keys.foldLeft(originalMap) { (acc, i) =>
      acc.updatedWith(i) {
        case None => Some(Set(value))
        case Some(s) => Some(s + value)
      }
    }
  }

  private def toposort(rems: List[CompiledReactive]): List[CompiledReactive] = {
    val sorted = ArrayBuffer[CompiledReactive]()
    val discovered = scala.collection.mutable.HashSet[CompiledReactive]()

    def _toposort(rem: CompiledReactive): Unit = {
      if discovered.contains(rem) then ()
      else {
        discovered += rem
        dataflow.getOrElse(rem, Set()).foreach(_toposort)
        sorted += rem
      }
    }

    rems.foreach(_toposort)
    sorted.toList
  }

  private val updateFunctions: List[CFunctionDecl] = reactives.map(_.updateFun)

  private val signalVariables: Map[CompiledSignal, CVarDecl] = signals.map {
    r => r -> CVarDecl(r.name, r.cType, None)
  }.toMap

  private val startup: CFunctionDecl =
    CFunctionDecl(
      "reactifiStartup",
      List(),
      CVoidType,
      Some(CCompoundStmt(
        signalVariables.collect {
          case (s@CompiledSignalExpr(_, updateFun, typeRepr), varDecl) =>
            CExprStmt(CAssignmentExpr(
              varDecl.ref,
              retain(
                CCallExpr(
                  updateFun.ref,
                  reactiveInputs(s).collect { case s: CompiledSignal => signalVariables(s).ref }
                ),
                typeRepr.asInstanceOf[TypeRepr]
              )
            ))
          case (CompiledFold(_, init, _, typeRepr), varDecl) =>
            CExprStmt(CAssignmentExpr(
              varDecl.ref,
              retain(init, typeRepr.asInstanceOf[TypeRepr])
            ))
        }.toList
      ))
    )

  private val sourceParameters: Map[CompiledReactive, CParmVarDecl] = sources.collect {
    case s: CompiledEvent if s.isSource => s -> CParmVarDecl(s.name, s.cType)
    case s: CompiledSignal if s.isSource => s ->
      CParmVarDecl(
        s.name + "_param",
        cascade.dispatch(_.compileTypeRepr)(TypeRepr.of[Option].appliedTo(s.typeRepr))
      )
  }.toMap

  private def eventVar(r: CompiledEvent): CVarDecl =
    CVarDecl(
      r.name,
      r.cType,
      Some(CDesignatedInitExpr(List("defined" -> CFalseLiteral)))
    )

  private val eventVariables: Map[CompiledEvent, CVarDecl] = events.collect {
    case r if r.isSource && cascade.dispatch(_.usesRefCount)(r.typeRepr) =>
      r -> eventVar(r).copy(name = r.name + "_copy")
    case r if !r.isSource && r.cType != CQualType(CVoidType) => r -> eventVar(r)
  }.toMap

  extension (expr: CExpr)
    def defined: CExpr = CMemberExpr(expr, "defined")
    def value: CExpr = CMemberExpr(expr, "val")

  private def conditionAfterFilter(r: CompiledReactive, cond: Map[CompiledReactive, Set[CExpr]]): Set[CExpr] = r match {
    case s if s.isSource => cond(s)
    case e: CompiledEvent if !e.alwaysPropagates => Set(eventVariables(e).ref.defined)
    case _ => cond(r)
  }

  private val updateConditions: Map[CompiledReactive, Set[CExpr]] =
    topological.foldLeft(Map.empty[CompiledReactive, Set[CExpr]]) { (acc, r) =>
      r match {
        case s if s.isSource => acc.updated(s, Set(sourceParameters(s).ref.defined))
        case _ => acc.updated(r, reactiveInputs(r).map(conditionAfterFilter(_, acc)).reduce(_ union _))
      }
    }

  private def compileCondition(conds: Set[CExpr]): CExpr = conds.toList match {
    case List(c) => c
    case l => l.reduce(COrExpr.apply)
  }

  private val topologicalByCond: List[CompiledReactive] = {
    val (groupedByCond: Map[Set[CExpr], List[CompiledReactive]], condOrder: List[Set[CExpr]]) =
      topological.foldLeft((Map.empty[Set[CExpr], List[CompiledReactive]], List[Set[CExpr]]())) {
        case ((groupedByCond, condOrder), r) =>
          val cond = updateConditions(r)
          if groupedByCond.contains(cond) then (groupedByCond.updated(cond, groupedByCond(cond) :+ r), condOrder)
          else (groupedByCond.updated(cond, List(r)), condOrder :+ cond)
      }

    condOrder.flatMap(groupedByCond)
  }

  private def valueRef(r: CompiledReactive): CDeclRefExpr =
    r match {
      case s: CompiledSignal => signalVariables(s).ref
      case s if s.isSource && !cascade.dispatch(_.usesRefCount)(s.typeRepr) => sourceParameters(s).ref
      case e: CompiledEvent => eventVariables(e).ref
    }

  private def compileUpdates(remainingReactives: List[CompiledReactive], toRelease: Set[CompiledReactive]): List[CStmt] = {
    if remainingReactives.isEmpty then return Nil

    val condition = updateConditions(remainingReactives.head)
    val (sameCond: List[CompiledReactive], otherConds: List[CompiledReactive]) =
      remainingReactives.span { r => updateConditions(r) == condition }

    def updateAssignment(reactive: CompiledReactive, rhs: CExpr, releaseAfter: Boolean = false): CStmt =
      val tempDecl = CVarDecl("temp", reactive.cType, Some(valueRef(reactive)))
      release(tempDecl.ref, reactive.typeRepr, CFalseLiteral) match {
        case Some(releaseStmt) if releaseAfter =>
          CCompoundStmt(List(
            tempDecl,
            CAssignmentExpr(
              valueRef(reactive),
              retain(rhs, reactive.typeRepr)
            ),
            releaseStmt
          ))
        case _ =>
          CAssignmentExpr(
            valueRef(reactive),
            retain(rhs, reactive.typeRepr)
          )
      }

    val updates = sameCond.collect {
      case s: CompiledEvent if s.isSource && cascade.dispatch(_.usesRefCount)(s.typeRepr) =>
        updateAssignment(s, deepCopy(sourceParameters(s).ref, s.typeRepr))
      case s: CompiledSignal if s.isSource =>
        updateAssignment(s, deepCopy(sourceParameters(s).ref.value, s.typeRepr))
      case f: CompiledFold =>
        updateAssignment(
          f,
          CCallExpr(
            f.updateFun.ref,
            valueRef(f) :: reactiveInputs(f).map(valueRef)
          ),
          true
        )
      case s: CompiledSignalExpr =>
        updateAssignment(
          s,
          CCallExpr(
            s.updateFun.ref,
            reactiveInputs(s).map(valueRef)
          ),
          true
        )
      case e: CompiledEvent if e.cType == CVoidType =>
        CExprStmt(CCallExpr(e.updateFun.ref, reactiveInputs(e).map(valueRef)))
      case e: CompiledEvent if !e.isSource =>
        updateAssignment(
          e,
          CCallExpr(
            e.updateFun.ref,
            reactiveInputs(e).map(valueRef)
          )
        )
    }

    val sameCondCode = CIfStmt(compileCondition(condition), CCompoundStmt(updates))

    val stillUsed = otherConds ++ otherConds.flatMap(reactiveInputs)
    val released = toRelease.filterNot(stillUsed.contains)

    val releaseCode = released.flatMap {
      case r if cascade.dispatch(_.usesRefCount)(r.typeRepr) =>
        CEmptyStmt :: (release(valueRef(r), r.typeRepr, CFalseLiteral) map { releaseStmt =>
          CIfStmt(valueRef(r).defined, releaseStmt)
        }).toList
      case _ => None
    }

    (CEmptyStmt :: sameCondCode :: releaseCode.toList) ++ compileUpdates(otherConds, toRelease.diff(released))
  }

  private val updateFunction: CFunctionDecl = {
    val params = sourcesTopological.map(sourceParameters)

    val localVarDecls = topological.collect {
      case e: CompiledEvent if eventVariables.contains(e) => CDeclStmt(eventVariables(e))
    }

    val updates = compileUpdates(topologicalByCond, eventVariables.keySet.toSet[CompiledReactive])

    val body = CCompoundStmt(localVarDecls ++ updates)

    CFunctionDecl("executeTransaction", params, CVoidType, Some(body))
  }

  private val mainC: String = appName + "Main.c"
  private val mainH: String = appName + "Main.h"
  private val mainInclude: CInclude = CInclude(mainH, true)
  private val libC: String = appName + "Lib.c"
  private val libH: String = appName + "Lib.h"
  private val libInclude: CInclude = CInclude(libH, true)
  private val appC: String = appName + "App.c"
  private val appOut: String = appName + "App"

  private val mainCTU: CTranslationUnitDecl = {
    val includes = mainInclude :: libInclude :: ctx.includesList

    val globalVarDecls = topological.collect {
      case s: CompiledSignal => signalVariables(s)
    }

    CTranslationUnitDecl(
      includes,
      globalVarDecls ++ updateFunctions ++ List(startup, updateFunction)
    )
  }

  private val mainHTU: CTranslationUnitDecl = {
    val includes = libInclude :: ctx.includesList

    val globalVarDecls = topological.collect {
      case s: CompiledSignal => signalVariables(s)
    }

    CTranslationUnitDecl(
      includes,
      globalVarDecls ++ List(startup.declOnly, updateFunction.declOnly),
      Some(appName.toUpperCase + "_MAIN")
    )
  }

  private def writeFile(path: String, content: String): Unit = {
    val file = new File(path)
    file.createNewFile()

    val fileWriter = new FileWriter(path)
    fileWriter.write(content)
    fileWriter.close()
  }

  private def writeMain(pathToDir: String): Unit = {
    writeFile(pathToDir + "/" + mainC, mainCTU.textgen)
    writeFile(pathToDir + "/" + mainH, mainHTU.textgen)
  }

  private val libCTU: CTranslationUnitDecl =
    CTranslationUnitDecl(
      libInclude :: ctx.includesList,
      ctx.valueDeclList.sortBy(_.name)
    )

  private val libHTU: CTranslationUnitDecl = {
    val valueDecls = ctx.valueDeclList.map(_.declOnly).sortBy(_.name)

    CTranslationUnitDecl(
      ctx.includesList,
      ctx.typeDeclList ++ valueDecls,
      Some(appName.toUpperCase + "_LIB")
    )
  }

  private def writeLib(pathToDir: String): Unit = {
    writeFile(pathToDir + "/" + libC, libCTU.textgen)
    writeFile(pathToDir + "/" + libH, libHTU.textgen)
  }

  private val appCTU: CTranslationUnitDecl =
    CTranslationUnitDecl(
      List(mainInclude, libInclude),
      List(CFunctionDecl(
        "main",
        List(),
        CIntegerType,
        Some(CCompoundStmt(List(CCallExpr(startup.ref, List()), CEmptyStmt, CReturnStmt(Some(0.lit)))))
      ))
    )

  private def writeApp(pathToDir: String): Unit = {
    writeFile(pathToDir + "/" + appC, appCTU.textgen)
  }

  private def writeMakeFile(pathToDir: String, compiler: String): Unit = {
    val localIncludes = ctx.includesList.filter(_.isLocal)

    val localFileString = localIncludes.flatMap { incl =>
      List(incl.name, incl.name.init + "c")
    }.mkString("", " ", "")

    val content =
      s"""
         |build:
         |\t$compiler $appC $mainC $mainH $libC $libH $localFileString -o $appOut
      """.strip().stripMargin

    writeFile(pathToDir + "/Makefile", content)
  }

  def writeIntoDir(pathToDir: String, compiler: String): Unit = {
    writeMain(pathToDir)
    writeLib(pathToDir)
    writeApp(pathToDir)
    writeMakeFile(pathToDir, compiler)
  }
}
