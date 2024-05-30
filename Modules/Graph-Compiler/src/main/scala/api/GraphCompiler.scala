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

class GraphCompiler(outputs: List[ReSource], mainFun: CMainFunction = CMainFunction.empty)(using
    hfc: HelperFunCollection
) {
  private val allNodes: Set[ReSource] = flattenGraph(outputs, Set())
  private val sources: Set[Source[?]] = allNodes.collect { case s: Source[_] => s }

  private val dataflow: Map[ReSource, Set[ReSource]] =
    allNodes.foldLeft(Map.empty[ReSource, Set[ReSource]]) { (acc, r) =>
      addValueToAllKeys(acc, r.inputs, r)
    }

  private val topological: List[ReSource]         = toposort(sources.toList).reverse
  private val sourcesTopological: List[Source[?]] = topological.collect { case s: Source[_] => s }

  @tailrec
  private def flattenGraph(check: List[ReSource], acc: Set[ReSource]): Set[ReSource] = {
    val more = check.filterNot(acc.contains)
    if more.nonEmpty then flattenGraph(more.flatMap(_.inputs), acc ++ more)
    else acc
  }

  private def addValueToAllKeys[K, V](originalMap: Map[K, Set[V]], keys: List[K], value: V): Map[K, Set[V]] = {
    keys.foldLeft(originalMap) { (acc, i) =>
      acc.updatedWith(i) {
        case None    => Some(Set(value))
        case Some(s) => Some(s + value)
      }
    }
  }

  private def toposort(rems: List[ReSource]): List[ReSource] = {
    val sorted     = ArrayBuffer[ReSource]()
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
    case Filter(_, _, f)   => List(f.node)
    case Map1(_, _, f)     => List(f.node)
    case Map2(_, _, _, f)  => List(f.node)
    case Fold(_, _, lines) => lines.map(_.f.node)
  }.flatten

  private val contexts: List[WithContext[?]] = topological.collect {
    case Source(_, tpe)         => List(tpe)
    case Map1(_, tpe, f)        => List(f, tpe)
    case Map2(_, _, tpe, f)     => List(f, tpe)
    case Filter(_, tpe, f)      => List(f, tpe)
    case Snapshot(_, _, tpe)    => List(tpe)
    case Or(_, _, tpe)          => List(tpe)
    case Fold(init, tpe, lines) => init :: tpe :: lines.map(_.f)
  }.flatten ++ hfc.helperFuns.map {
    case WithContext(f, includes, recordDecls, functionDecls) =>
      WithContext(f, includes, recordDecls, f :: functionDecls)
  } appended mainFun.f

  private val globalVariables: Map[Fold[?], CVarDecl] = Map.from(allNodes.collect {
    case f @ Fold(_, cType, _) => f -> CVarDecl(f.valueName, cType.node, None)
  })

  private def retain(expr: CExpr, tpe: WithContext[CType]): CExpr = tpe.node match {
    case CRecordType(declName) =>
      val funName = "retain_" + declName

      tpe.valueDecls.collectFirst {
        case f @ CFunctionDecl(`funName`, _, _, _, _) => CCallExpr(f.ref, List(expr))
      }.getOrElse(expr)
    case _ => expr
  }

  private def release(expr: CExpr, tpe: WithContext[CType], keepWithZero: Boolean = false): Option[CStmt] =
    tpe.node match {
      case CRecordType(declName) =>
        val funName = "release_" + declName

        tpe.valueDecls.collectFirst {
          case f @ CFunctionDecl(`funName`, _, _, _, _) =>
            CCallExpr(f.ref, List(expr, if keepWithZero then CTrueLiteral else CFalseLiteral))
        }
      case _ => None
    }

  private def usesRefCount(tpe: WithContext[CType]): Boolean = tpe.node match {
    case CRecordType(declName) =>
      val funName = "release_" + declName

      tpe.valueDecls.exists {
        case CFunctionDecl(`funName`, _, _, _, _) => true
        case _                                    => false
      }
    case _ => false
  }

  private def createSome(expr: CExpr, tpe: WithContext[CType]): CExpr = tpe.node match {
    case CRecordType(declName) =>
      val funName = "createSome_" + declName

      tpe.valueDecls.collectFirst {
        case f @ CFunctionDecl(`funName`, _, _, _, _) => CCallExpr(f.ref, List(expr))
      }.get
    case _ => throw new MatchError(tpe)
  }

  private def createNone(tpe: WithContext[CType]): CExpr = tpe.node match {
    case CRecordType(declName) =>
      val funName = "createNone_" + declName

      tpe.valueDecls.collectFirst {
        case f @ CFunctionDecl(`funName`, _, _, _, _) => CCallExpr(f.ref, List())
      }.get
    case _ => throw new MatchError(tpe)
  }

  private def activeVal(r: ReSource): CExpr = r match {
    case f @ Fold(_, _, _) => valueRef(f)
    case _                 => CMemberExpr(valueRef(r), "val")
  }

  private def optionVal(r: ReSource, cType: WithContext[CType]): CExpr = r match {
    case f @ Fold(_, _, _) => createSome(valueRef(f), cType)
    case _                 => valueRef(r)
  }

  private def dotDefined(expr: CExpr): CExpr = CMemberExpr(expr, "defined")

  private def deepCopy(expr: CExpr, tpe: WithContext[CType]): CExpr = tpe.node match {
    case CRecordType(declName) =>
      val deepCopyFunName = "deepCopy_" + declName

      tpe.valueDecls.collectFirst {
        case f @ CFunctionDecl(`deepCopyFunName`, _, _, _, _) => f
      } match {
        case Some(deepCopyFun) => CCallExpr(deepCopyFun.ref, List(expr))
        case None              => expr
      }
    case _ => expr
  }

  private val startup: CFunctionDecl =
    CFunctionDecl(
      "reactifiStartup",
      List(),
      CVoidType,
      Some(CCompoundStmt(
        globalVariables.map {
          case (Fold(init, tpe, _), varDecl) =>
            CExprStmt(CAssignmentExpr(
              varDecl.ref,
              retain(init.node, tpe)
            ))
        }.toList
      ))
    )

  private val sourceParameters: Map[Source[?], CParmVarDecl] = Map.from(allNodes.collect {
    case s @ Source(_, cType) => s -> CParmVarDecl(s.valueName, cType.node)
  })

  private def eventVar(r: ReSource): CVarDecl =
    CVarDecl(
      r.valueName,
      r.cType.node,
      Some(CDesignatedInitExpr(List("defined" -> CFalseLiteral)))
    )

  private val localVariables: Map[ReSource, CVarDecl] = Map.from(allNodes.collect {
    case r @ Source(_, cType) if usesRefCount(cType) => r -> eventVar(r).copy(name = r.valueName + "_copy")
    case r @ Map1(_, _, f) if f.node.returnType != CQualType(CVoidType) => r -> eventVar(r)
    case r @ Map2(_, _, _, _)                                           => r -> eventVar(r)
    case r @ Filter(_, _, _)                                            => r -> eventVar(r)
    case r @ Snapshot(_, _, _)                                          => r -> eventVar(r)
    case r @ Or(_, _, _)                                                => r -> eventVar(r)
  })

  private val updateConditions: Map[ReSource, UpdateCondition] = {
    val m = topological.foldLeft(Map.empty[ReSource, UpdateCondition]) { (acc, r) =>
      r match {
        case s @ Source(_, _)            => acc.updated(s, UpdateCondition(dotDefined(sourceParameters(s).ref)))
        case m @ Map1(input, _, _)       => acc.updated(m, acc(input))
        case m @ Map2(left, right, _, _) => acc.updated(m, acc(left).and(acc(right)))
        case f @ Filter(_, _, _)         => acc.updated(f, UpdateCondition(dotDefined(valueRef(f))))
        case s @ Snapshot(e, _, _)       => acc.updated(s, acc(e))
        case o @ Or(left, right, _)      => acc.updated(o, acc(left).or(acc(right)))
        case f @ Fold(_, _, _) =>
          acc.updated(f, f.inputs.foldLeft(UpdateCondition.empty) { (cond, input) => cond.or(acc(input)) })
      }
    }

    m.map {
      case (f: Filter[_], _) => f -> m(f.input)
      case other             => other
    }
  }

  private def valueRef(r: ReSource): CDeclRefExpr =
    r match {
      case s @ Source(_, cType) if !usesRefCount(cType) => sourceParameters(s).ref
      case f: Fold[_]                                   => globalVariables(f).ref
      case _                                            => localVariables(r).ref
    }

  private def compileUpdates(reSources: List[ReSource], toRelease: Set[ReSource]): List[CStmt] = {
    if reSources.isEmpty then return Nil

    val condition             = updateConditions(reSources.head)
    val (sameCond, otherCond) = reSources.span { r => updateConditions(r) == condition }

    def updateAssignment(
        reSource: ReSource,
        declaredType: WithContext[CType],
        rhs: CExpr,
        releaseAfter: Boolean = false
    ): CStmt =
      val tempDecl = CVarDecl("temp", declaredType.node, Some(valueRef(reSource)))
      release(tempDecl.ref, declaredType) match {
        case Some(releaseStmt) if releaseAfter =>
          CCompoundStmt(List(
            tempDecl,
            CAssignmentExpr(
              valueRef(reSource),
              retain(rhs, declaredType)
            ),
            releaseStmt
          ))
        case _ =>
          CAssignmentExpr(
            valueRef(reSource),
            retain(rhs, declaredType)
          )
      }

    val updates = sameCond.flatMap {
      case r @ Source(_, cType) if usesRefCount(cType) =>
        List(updateAssignment(
          r,
          cType,
          deepCopy(sourceParameters(r).ref, cType)
        ))
      case Source(_, _) => Nil
      case Map1(input, _, f) if f.node.returnType == CQualType(CVoidType) =>
        List[CStmt](CCallExpr(f.node.ref, List(activeVal(input))))
      case r @ Map1(input, cType, f) =>
        List(updateAssignment(
          r,
          cType,
          createSome(CCallExpr(f.node.ref, List(activeVal(input))), cType)
        ))
      case r @ Map2(left, right, cType, f) =>
        List(updateAssignment(
          r,
          cType,
          CCallExpr(f.node.ref, List(activeVal(left), activeVal(right)))
        ))
      case r @ Filter(input, cType, f) =>
        List(CIfStmt(
          CCallExpr(f.node.ref, List(activeVal(input))),
          updateAssignment(r, cType, optionVal(input, cType)),
          Some(updateAssignment(r, cType, createNone(cType)))
        ))
      case r @ Snapshot(_, input, cType) =>
        List(updateAssignment(
          r,
          cType,
          optionVal(input, cType)
        ))
      case r @ Or(left, right, cType) =>
        List(updateAssignment(
          r,
          cType,
          CConditionalOperator(
            updateConditions(left).compile,
            optionVal(left, cType),
            optionVal(right, cType)
          )
        ))
      case r @ Fold(_, cType, lines) => lines match {
          case List(FLine(input, f)) =>
            List(updateAssignment(
              r,
              cType,
              CCallExpr(f.node.ref, List(activeVal(r), activeVal(input))),
              true
            ))
          case _ =>
            lines.map {
              case FLine(input, f) =>
                CIfStmt(
                  updateConditions(input).compile,
                  updateAssignment(
                    r,
                    cType,
                    CCallExpr(f.node.ref, List(activeVal(r), activeVal(input))),
                    true
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

    val stillUsed = otherCond.flatMap(_.inputs)
    val released  = toRelease.filterNot(stillUsed.contains)

    val releaseCode = released.flatMap {
      case resource if usesRefCount(resource.cType) =>
        CEmptyStmt :: (release(valueRef(resource), resource.cType) map { releaseStmt =>
          CIfStmt(dotDefined(valueRef(resource)), releaseStmt)
        }).toList
      case _ => None
    }

    (CEmptyStmt :: sameCondCode :: releaseCode.toList) ++ compileUpdates(otherCond, toRelease.diff(released))
  }

  private val updateFunction: CFunctionDecl = {
    val params = sourcesTopological.map(sourceParameters)

    val localVarDecls = topological.flatMap(localVariables.get).map(CDeclStmt.apply)

    val updates = compileUpdates(topological, localVariables.keySet)

    val body =
      CCompoundStmt(
        localVarDecls ++ updates
      )

    CFunctionDecl("executeReactifiUpdate", params, CVoidType, Some(body))
  }

  def forUseInHeader(valueDecl: CValueDecl): CValueDecl = valueDecl match
    case cvd: CVarDecl => cvd.copy(inHeader = true)
    case other         => other

  private val mainC: String         = "reactifiMain.c"
  private val mainH: String         = "reactifiMain.h"
  private val mainInclude: CInclude = CInclude(mainH, true)
  private val libC: String          = "reactifiLib.c"
  private val libH: String          = "reactifiLib.h"
  private val libInclude: CInclude  = CInclude(libH, true)
  private val appC: String          = "reactifiApp.c"
  private val appOut: String        = "reactifiApp"

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
    }.map(forUseInHeader)

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

    val valueDecls = Set.from(contexts.flatMap(_.valueDecls)).toList.sortBy(_.name)

    CTranslationUnitDecl(
      includes,
      valueDecls
    )
  }

  private val libHTU: CTranslationUnitDecl = {
    val includes = Set.from(contexts.flatMap(_.includes)).toList

    val typeDecls  = appendWithoutDuplicates(contexts.map(_.typeDecls))
    val valueDecls = Set.from(contexts.flatMap(_.valueDecls)).toList.map(_.declOnly).sortBy(_.name).map(forUseInHeader)

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
              case (sourceName, expr) if sourceName.equals(source.name) => createSome(expr, source.cType)
            }.getOrElse(createNone(source.cType))

            CVarDecl(source.valueName, source.cType.node, Some(initExpr))
          }

          val updateCall = CCallExpr(updateFunction.ref, tempDecls.map(_.ref))

          val releases = tempDecls.zip(sourcesTopological).collect {
            case (v, Source(_, cType)) if usesRefCount(cType) =>
              release(v.ref, cType).get
          }

          CCompoundStmt(tempDecls.map(CDeclStmt.apply) ++ (updateCall :: releases))
      }
    }

    val transformedMainFun = transactionMapper.mapCValueDecl(mainFun.f.node) match {
      case f @ CFunctionDecl(_, _, _, Some(CCompoundStmt(body)), _) =>
        val releaseStmts: List[CStmt] = topological.collect {
          case f @ Fold(_, cType, _) if usesRefCount(cType) =>
            release(globalVariables(f).ref, cType).get
        }

        val transformedBody: List[CStmt] =
          (CExprStmt(CCallExpr(startup.ref, List())) ::
            body.init ++ releaseStmts) :+ body.last

        f.copy(body = Some(CCompoundStmt(transformedBody)))
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
    val localIncludes = Set.from(contexts.flatMap(_.includes.filter(_.isLocal))).toList

    val localFileString = localIncludes.flatMap { incl =>
      List(incl.name, incl.name.init + "c")
    }.mkString(" ", " ", "")

    val content =
      s"""
         |build:
         |\t$compiler $appC $mainC $mainH $libC $libH$localFileString -o $appOut
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
