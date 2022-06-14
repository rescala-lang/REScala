package api

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.*
import clangast.stmt.*
import clangast.stubs.StdBoolH
import clangast.traversal.CASTMapper
import clangast.types.*
import compiler.ext.CTransactionStatement

import java.io.{File, FileWriter}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class GraphCompiler(outputs: List[ReSource], mainFun: CMainFunction = CMainFunction.empty)(using hfc: HelperFunCollection) {
  private val allNodes: Set[ReSource] = flattenGraph(outputs, Set())
  private val sources: Set[Source[_]] = allNodes.collect { case s: Source[_] => s }

  private val dataflow: Map[ReSource, Set[ReSource]] =
    allNodes.foldLeft(Map.empty[ReSource, Set[ReSource]]) { (acc, r) =>
      addValueToAllKeys(acc, r.inputs, r)
    }

  private val topological: List[ReSource] = toposort(sources.toList).reverse
  private val sourcesTopological: List[Source[_]] = topological.collect { case s: Source[_] => s }

  @tailrec
  private def flattenGraph(check: List[ReSource], acc: Set[ReSource]): Set[ReSource] = {
    val more = check.filterNot(acc.contains)
    if more.nonEmpty then flattenGraph(more.flatMap(_.inputs), acc ++ more)
    else acc
  }

  private def addValueToAllKeys[K, V](originalMap: Map[K, Set[V]], keys: List[K], value: V): Map[K, Set[V]] = {
    keys.foldLeft(originalMap) { (acc, i) =>
      acc.updatedWith(i) {
        case None => Some(Set(value))
        case Some(s) => Some(s + value)
      }
    }
  }

  private def toposort(rems: List[ReSource]): List[ReSource] = {
    val sorted = ArrayBuffer[ReSource]()
    val discovered = scala.collection.mutable.HashSet[ReSource]()

    def _toposort(rem: ReSource): Unit = {
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

  private val functionDefinitions: List[CFunctionDecl] = topological.collect {
    case Filter(_, f) => List(f.node)
    case Map1(_, _, f) => List(f.node)
    case Map2(_, _, _, f) => List(f.node)
    case Fold(_, _, lines) => lines.map(_.f.node)
  }.flatten

  private val contexts: List[WithContext[?]] = topological.collect {
    case Source(_, tpe) => List(tpe)
    case Map1(_, _, f) => List(f)
    case Map2(_, _, _, f) => List(f)
    case Filter(_, f) => List(f)
    case Fold(init, _, lines) => init :: lines.map(_.f)
  }.flatten ++ hfc.helperFuns.map {
    case WithContext(f, includes, recordDecls, functionDecls) =>
      WithContext(f, includes, recordDecls, f :: functionDecls)
  } appended mainFun.f

  private val globalVariables: Map[Fold[?], CVarDecl] = Map.from(allNodes.collect {
    case f@Fold(_, cType, _) => f -> CVarDecl(f.valueName, cType.node, None)
  })

  private val startup: CFunctionDecl =
    CFunctionDecl(
      "reactifiStartup",
      List(),
      CVoidType,
      Some(CCompoundStmt(
        globalVariables.map {
          case (Fold(init, _, _), varDecl) =>
            CExprStmt(CAssignmentExpr(
              CDeclRefExpr(varDecl),
              init.node
            ))
        }.toList
      ))
    )

  private val sourceValueParameters: Map[Source[?], CParmVarDecl] = Map.from(allNodes.collect {
    case s@Source(_, cType) => s -> CParmVarDecl(s.valueName, cType.node)
  })

  private val sourceValidParameters: Map[Source[?], CParmVarDecl] = Map.from(allNodes.collect {
    case s@Source(_, _) => s -> CParmVarDecl(s.validName, CBoolType)
  })

  private val localVariables: Map[ReSource, CVarDecl] = Map.from(allNodes.collect {
    case r@Map1(_, cType, f) if f.node.returnType != CQualType(CVoidType) => r -> CVarDecl(r.valueName, cType.node)
    case r@Map2(_, _, cType, _) => r -> CVarDecl(r.valueName, cType.node)
    case r@Filter(_, _) => r -> CVarDecl(r.valueName, CBoolType)
    case r@Or(_, _, cType) => r -> CVarDecl(r.valueName, cType.node)
  })

  private val updateConditions: Map[ReSource, UpdateCondition] = {
    topological.foldLeft(Map.empty[ReSource, UpdateCondition]) { (acc, r) =>
      r match {
        case s@Source(_, _) => acc.updated(s, UpdateCondition(sourceValidParameters(s)))
        case m@Map1(input, _, _) => acc.updated(m, acc(input))
        case m@Map2(left, right, _, _) => acc.updated(m, acc(left).and(acc(right)))
        case f@Filter(input, _) => acc.updated(f, acc(input).add(localVariables(f)))
        case s@Snapshot(input, _) => acc.updated(s, acc(input))
        case o@Or(left, right, _) => acc.updated(o, acc(left).or(acc(right)))
        case f@Fold(_, _, _) =>
          acc.updated(f, f.inputs.foldLeft(UpdateCondition.empty) { (cond, input) => cond.or(acc(input)) })
      }
    }.map {
      case (f: Filter[_], cond) => f -> UpdateCondition(cond.normalized.map(_.filter(_ != localVariables(f))))
      case other => other
    }
  }

  @tailrec
  private def valueRef(r: ReSource): CDeclRefExpr =
    r match {
      case s: Source[_] => CDeclRefExpr(sourceValueParameters(s))
      case f: Fold[_] => CDeclRefExpr(globalVariables(f))
      case Filter(input, _) => valueRef(input)
      case Snapshot(_, fold) => valueRef(fold)
      case _ => CDeclRefExpr(localVariables(r))
    }

  // TODO: free allocated memory
  private def compileUpdates(reSources: List[ReSource]): List[CStmt] = {
    if (reSources.isEmpty) return Nil

    val condition = updateConditions(reSources.head)
    val (sameCond, otherCond) = reSources.span { r => updateConditions(r) == condition }

    def updateAssignment(target: CExpr, rhs: CExpr): CStmt =
      CExprStmt(
        CAssignmentExpr(
          target,
          rhs
        )
      )

    val updates = sameCond.flatMap {
      case Map1(input, _, f) if f.node.returnType == CQualType(CVoidType) =>
        List(CExprStmt(CCallExpr(CDeclRefExpr(f.node), List(valueRef(input)))))
      case r@Map1(input, _, f) =>
        List(updateAssignment(
          valueRef(r),
          CCallExpr(CDeclRefExpr(f.node), List(valueRef(input)))
        ))
      case r@Map2(left, right, _, f) =>
        List(updateAssignment(
          valueRef(r),
          CCallExpr(CDeclRefExpr(f.node), List(valueRef(left), valueRef(right)))
        ))
      case r@Filter(input, f) =>
        List(updateAssignment(
          CDeclRefExpr(localVariables(r)),
          CCallExpr(CDeclRefExpr(f.node), List(valueRef(input)))
        ))
      case Snapshot(_, _) => List()
      case r@Or(left, right, _) =>
        List(updateAssignment(
          valueRef(r),
          CConditionalOperator(
            updateConditions(left).compile,
            valueRef(left),
            valueRef(right)
          )
        ))
      case r@Fold(_, _, lines) => lines match {
        case List(FLine(input, f)) =>
          List(updateAssignment(
            valueRef(r),
            CCallExpr(CDeclRefExpr(f.node), List(valueRef(r), valueRef(input)))
          ))
        case _ =>
          lines.map {
            case FLine(input, f) =>
              CIfStmt(
                updateConditions(input).compile,
                updateAssignment(
                  valueRef(r),
                  CCallExpr(CDeclRefExpr(f.node), List(valueRef(r), valueRef(input)))
                )
              )
          }
      }
    }

    val sameCondCode =
      CIfStmt(
        condition.compile,
        CCompoundStmt(updates)
      )

    if (otherCond.isEmpty) List(sameCondCode)
    else sameCondCode :: compileUpdates(otherCond)
  }

  private val updateFunction: CFunctionDecl = {
    val params = sourcesTopological.flatMap { s => List(sourceValueParameters(s), sourceValidParameters(s)) }

    val localVarDecls = topological.flatMap(localVariables.get).map(CDeclStmt.apply)
    val updates = compileUpdates(topological.filterNot(_.isInstanceOf[Source[?]]))

    val body =
      CCompoundStmt(
        localVarDecls ++ updates
      )

    CFunctionDecl("executeReactifiUpdate", params, CVoidType, Some(body))
  }

  private val mainC: String = "reactifiMain.c"
  private val mainH: String = "reactifiMain.h"
  private val mainInclude: CInclude = CInclude(mainH, true)
  private val libC: String = "reactifiLib.c"
  private val libH: String = "reactifiLib.h"
  private val libInclude: CInclude = CInclude(libH, true)
  private val appC: String = "reactifiApp.c"
  private val appOut: String = "reactifiApp"

  private val mainCTU: CTranslationUnitDecl = {
    val includes = mainInclude :: libInclude :: Set.from(StdBoolH.include :: contexts.flatMap(_.includes)).toList

    val globalVarDecls = topological.collect {
      case f: Fold[_] => globalVariables(f)
    }

    CTranslationUnitDecl(
      includes,
      globalVarDecls ++ functionDefinitions ++ List(startup, updateFunction)
    )
  }

  private val mainHTU: CTranslationUnitDecl = {
    val includes = libInclude :: Set.from(contexts.flatMap(_.includes)).toList

    val globalVarDecls = topological.collect {
      case f: Fold[_] => globalVariables(f).declOnly
    }

    CTranslationUnitDecl(
      includes,
      globalVarDecls ++ List(startup.declOnly, updateFunction.declOnly),
      Some("REACTIFI_MAIN")
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

  private def appendWithoutDuplicates[T](l: List[List[T]]): List[T] = l.foldLeft((List.empty[T], Set.empty[T])) {
    case ((accList, accSet), innerList) =>
      (accList ++ innerList.filterNot(accSet.contains), accSet ++ innerList)
  }._1

  private val libCTU: CTranslationUnitDecl = {
    val includes = libInclude :: Set.from(contexts.flatMap(_.includes)).toList

    val valueDecls = Set.from(contexts.flatMap(_.valueDecls)).toList

    CTranslationUnitDecl(
      includes,
      valueDecls
    )
  }

  private val libHTU: CTranslationUnitDecl = {
    val includes = Set.from(contexts.flatMap(_.includes)).toList

    val typeDecls = appendWithoutDuplicates(contexts.map(_.typeDecls))
    val valueDecls = Set.from(contexts.flatMap(_.valueDecls)).toList.map(_.declOnly)

    CTranslationUnitDecl(
      includes,
      typeDecls ++ valueDecls,
      Some("REACTIFI_LIB")
    )
  }

  private def writeLib(pathToDir: String): Unit = {
    writeFile(pathToDir + "/" + libC, libCTU.textgen)
    writeFile(pathToDir + "/" + libH, libHTU.textgen)
  }

  private val appCTU: CTranslationUnitDecl = {
    val includes = mainInclude :: libInclude :: mainFun.f.includes

    val transactionMapper = new CASTMapper {
      override protected val mapCStmtHook: PartialFunction[CStmt, CStmt] = {
        case CTransactionStatement(args) =>
          val tempDecls = sourcesTopological.map { source =>
            val initExpr = args.collectFirst {
              case (sourceName, expr) if sourceName.equals(source.name) => expr
            }

            CVarDecl(source.valueName, source.cType.node, initExpr)
          }

          val updateCall = CCallExpr(
            CDeclRefExpr(updateFunction),
            tempDecls.flatMap { varDecl => List(CDeclRefExpr(varDecl), if varDecl.init.isDefined then CTrueLiteral else CFalseLiteral) }
          )

          CCompoundStmt(tempDecls.map(CDeclStmt.apply).appended(updateCall))
      }
    }

    val transformedMainFun = transactionMapper.mapCValueDecl(mainFun.f.node) match {
      case f @ CFunctionDecl(_, _, _, Some(CCompoundStmt(body)), _) =>
        f.copy(body = Some(CCompoundStmt(CCallExpr(CDeclRefExpr(startup), List()) :: body)))
    }

    CTranslationUnitDecl(
      includes,
      List(transformedMainFun)
    )
  }

  private def writeApp(pathToDir: String): Unit = {
    writeFile(pathToDir + "/" + appC, appCTU.textgen)
  }

  private def writeMakeFile(pathToDir: String, compiler: String): Unit = {
    val content =
      s"""
         |build:
         |\t$compiler $appC $mainC $mainH $libC $libH -o $appOut
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
