package macros

import clangast.*
import clangast.given
import clangast.decl.*
import clangast.expr.*
import clangast.expr.binaryop.*
import clangast.expr.unaryop.*
import clangast.stmt.*
import clangast.types.*

import scala.annotation.tailrec
import scala.quoted.*

object ScalaToC {
  def scalaToCCode[T](expr: Expr[T], funName: Expr[String])(using Quotes): Expr[CASTNode] = {
    import quotes.reflect.*

    println(expr.asTerm.show(using Printer.TreeStructure))

    val cast = compileTree(expr.asTerm, new TranslationContext()) match {
      case funDecl: CFunctionDecl => funDecl.copy(name = funName.value.get)
      case astNode => astNode
    }

    println(cast)
    println(cast.textgen)

    cast.toExpr
  }

  inline def scalaToC(inline funName: String)(inline expr: Any): CASTNode = ${ scalaToCCode('expr, 'funName) }

  def compileTree(using Quotes)(tree: quotes.reflect.Tree, ctx: TranslationContext): CASTNode = {
    import quotes.reflect.*

    tree match {
      case statement: Statement => compileStatement(statement, ctx)
      case typeTree: TypeTree => compileTypeRepr(typeTree.tpe, ctx)
      case _ => throw new MatchError(tree.show(using Printer.TreeStructure))
    }
  }

  def compileStatement(using Quotes)(statement: quotes.reflect.Statement, ctx: TranslationContext): CASTNode = {
    import quotes.reflect.*

    statement match {
      case definition: Definition => compileDefinition(definition, ctx)
      case term: Term => compileTerm(term, ctx)
      case _ => throw new MatchError(statement.show(using Printer.TreeStructure))
    }
  }

  def compileStatementToCStmt(using Quotes)(statement: quotes.reflect.Statement, ctx: TranslationContext): CStmt = {
    import quotes.reflect.*

    statement match {
      case definition: Definition => CDeclStmt(compileDefinition(definition, ctx))
      case term: Term => compileTermToCStmt(term, ctx)
      case _ => throw new MatchError(statement.show(using Printer.TreeStructure))
    }
  }

  def compileDefinition(using Quotes)(definition: quotes.reflect.Definition, ctx: TranslationContext): CDecl = {
    import quotes.reflect.*

    definition match {
      case defDef: DefDef => compileDefDef(defDef, ctx)
      case valDef: ValDef => compileValDefToCVarDecl(valDef, ctx)
      case _ => throw new MatchError(definition.show(using Printer.TreeStructure))
    }
  }

  def compileDefDef(using Quotes)(defDef: quotes.reflect.DefDef, ctx: TranslationContext): CFunctionDecl = {
    import quotes.reflect.*

    val DefDef(name, _, returnTpt, rhs) = defDef

    val params = defDef.termParamss.flatMap(_.params)

    val compiledParams = params.map(compileValDefToCParmVarDecl(_, ctx))

    val body = rhs.map { (t: Term) => t match
      case block: Block => compileBlockToFunctionBody(block, ctx)
      case Return(expr, _) => CCompoundStmt(List(CReturnStmt(Some(compileTermToCExpr(expr, ctx)))))
      case term => CCompoundStmt(List(CReturnStmt(Some(compileTermToCExpr(term, ctx)))))
    }

    val decl = CFunctionDecl(name, compiledParams, compileTypeRepr(returnTpt.tpe, ctx), body)

    ctx.nameToDecl.put(name, decl)

    decl
  }

  def compileValDefToCVarDecl(using Quotes)(valDef: quotes.reflect.ValDef, ctx: TranslationContext): CVarDecl = {
    import quotes.reflect.*

    val ValDef(name, tpt, rhs) = valDef

    val decl = CVarDecl(name, compileTypeRepr(tpt.tpe, ctx), rhs.map(compileTermToCExpr(_, ctx)))

    ctx.nameToDecl.put(name, decl)

    decl
  }

  def compileValDefToCParmVarDecl(using Quotes)(valDef: quotes.reflect.ValDef, ctx: TranslationContext): CParmVarDecl = {
    import quotes.reflect.*

    val ValDef(name, tpt, _) = valDef

    val decl = CParmVarDecl(name, compileTypeRepr(tpt.tpe, ctx))

    ctx.nameToDecl.put(name, decl)

    decl
  }

  @tailrec
  def compileTerm(using Quotes)(term: quotes.reflect.Term, ctx: TranslationContext): CASTNode = {
    import quotes.reflect.*

    term match {
      case ref: Ref => compileRef(ref, ctx)
      case literal: Literal => compileLiteral(literal, ctx)
      case apply: Apply => compileApply(apply, ctx)
      case assign: Assign => compileAssign(assign, ctx)
      case Block(List(defDef: DefDef), _: Closure) => compileDefDef(defDef, ctx)
      case Block(List(defDef: DefDef), Literal(UnitConstant())) => compileDefDef(defDef, ctx)
      case block: Block => compileBlockToCCompoundStmt(block, ctx)
      case ifTerm: If => compileIfToCIfStmt(ifTerm, ctx)
      case ret: Return => compileReturn(ret, ctx)
      case inlined: Inlined => compileTerm(inlined.underlyingArgument, ctx)
      case whileTerm: While => compileWhile(whileTerm, ctx)
      case typed: Typed => compileTerm(typed.expr, ctx)
      case _ => throw new MatchError(term.show(using Printer.TreeStructure))
    }
  }

  def compileTermToCStmt(using Quotes)(term: quotes.reflect.Term, ctx: TranslationContext): CStmt = {
    import quotes.reflect.*

    compileTerm(term, ctx) match {
      case stmt: CStmt => stmt
      case expr: CExpr => CExprStmt(expr)
      case _ => throw new MatchError(term.show(using Printer.TreeStructure))
    }
  }

  def compileTermToCExpr(using Quotes)(term: quotes.reflect.Term, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    term match {
      case block: Block => compileBlockToCStmtExpr(block, ctx)
      case ifTerm: If => compileIfToCConditionalOperator(ifTerm, ctx)
      case _ =>
        compileTerm(term, ctx) match {
          case expr: CExpr => expr
          case _ => throw new MatchError(term.show(using Printer.TreeStructure))
        }
    }
  }

  def compileRef(using Quotes)(ref: quotes.reflect.Ref, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    ref match {
      case ident: Ident => compileIdent(ident, ctx)
      case select: Select => compileSelect(select, ctx)
      case _ => throw new MatchError(ref.show(using Printer.TreeStructure))
    }
  }

  def compileIdent(using Quotes)(ident: quotes.reflect.Ident, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    ctx.nameToDecl.get(ident.name) match
      // possibly pointer instead of direct reference
      case Some(decl) => CDeclRefExpr(decl)
      // (maybe?) if no decl exists in ctx, use an unchecked string-based reference instead
      // can't create a new variable declaration because I don't know how to follow an ident to its definition
      case None => throw new MatchError(ident.show(using Printer.TreeStructure))
  }

  def compileSelect(using Quotes)(select: quotes.reflect.Select, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    // Select with function calls should already be handled by outer apply
    // Check that this select is either on a case class or tuple
    // compile to a MemberExpr using a struct definition from the ctx
    // if no struct definition exists yet, create a new one

    val Select(qualifier, name) = select

    if (canCompileToCUnaryOperator(qualifier, name)) {
      compileSelectToCUnaryOperator(select, ctx)
    } else if (isProductFieldAccess(qualifier, name)) {
      val recordDecl = getRecordDecl(qualifier.tpe, ctx)

      // When is the arrow necessary?
      CMemberExpr(compileTermToCExpr(qualifier, ctx), recordDecl.fields.find(_.name.equals(name)).get)
    } else {
      throw new MatchError(select.show(using Printer.TreeStructure))
    }
  }

  def canCompileToCUnaryOperator(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    val unaryArithmeticOperators = List("unary_+", "unary_-")
    val unaryBitwiseOperators = List("unary_~")
    val unaryLogicalOperators = List("unary_!")

    val numberUnaryOperators = unaryArithmeticOperators ++ unaryBitwiseOperators
    val booleanUnaryOperators = unaryLogicalOperators

    isNumberType(term.tpe) && numberUnaryOperators.contains(name) ||
      term.tpe <:< TypeRepr.of[Boolean] && booleanUnaryOperators.contains(name)
  }

  def compileSelectToCUnaryOperator(using Quotes)(select: quotes.reflect.Select, ctx: TranslationContext): CUnaryOperator = {
    import quotes.reflect.*

    val Select(qualifier, name) = select

    val subExpr = compileTermToCExpr(qualifier, ctx)

    name match {
      case "unary_+" => CUnaryPlusExpr(CParenExpr(subExpr))
      case "unary_-" => CUnaryMinusExpr(CParenExpr(subExpr))
      case "unary_~" => CBitwiseNotExpr(CParenExpr(subExpr))
      case "unary_!" => CNotExpr(CParenExpr(subExpr))
      case _ => throw new MatchError(select.show(using Printer.TreeStructure))
    }
  }

  def isProductFieldAccess(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    (term.tpe <:< TypeRepr.of[Product]) && term.tpe.classSymbol.get.caseFields.exists(_.name.equals(name))
  }

  def compileLiteral(using Quotes)(literal: quotes.reflect.Literal, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    literal match {
      case Literal(BooleanConstant(false)) => CFalseLiteral
      case Literal(BooleanConstant(true)) => CTrueLiteral
      case Literal(ByteConstant(x)) => CIntegerLiteral(x)
      case Literal(ShortConstant(x)) => CIntegerLiteral(x)
      case Literal(IntConstant(x)) => CIntegerLiteral(x)
      case Literal(LongConstant(x)) => CLongLiteral(x)
      case Literal(FloatConstant(x)) => CFloatLiteral(x)
      case Literal(DoubleConstant(x)) => CDoubleLiteral(x)
      case Literal(CharConstant(x)) => CCharacterLiteral(x)
      case Literal(StringConstant(x)) => CStringLiteral(x)
      case Literal(NullConstant()) => CNullLiteral
      case _ => throw new MatchError(literal.show(using Printer.TreeStructure))
    }
  }

  def compileApply(using Quotes)(apply: quotes.reflect.Apply, ctx: TranslationContext): CExpr = {
    import quotes.reflect.*

    // There is no general case here, instead just multiple special cases for different types of functions applied
    // such as those that can be translated to operator and methods on standard data structures that need to be given
    // as C code

    apply match {
      case Apply(Ident("println"), List(arg)) =>
        // needs unlinked CCallStmt (name of function instead of reference)
        // alternatively, provide mock definitions for library functions
        compileTermToCExpr(arg, ctx)
      case Apply(Select(Select(Ident("math"), "package"), "max"), List(arg1, arg2)) =>
        // Should compile to StmtExpr instead where arg1 and arg2 are only evaluated once each
        // Needs translation of types

        val expr1 = compileTermToCExpr(arg1, ctx)
        val expr2 = compileTermToCExpr(arg2, ctx)

        CConditionalOperator(
          CGreaterThanExpr(expr1, expr2),
          expr1,
          expr2
        )
      case Apply(Select(_, "apply"), l) if isProductApply(apply) =>
        CCallExpr(CDeclRefExpr(getRecordCreator(apply.tpe, ctx)), l.map(compileTermToCExpr(_, ctx)))
      case Apply(TypeApply(Select(_, "apply"), _), l) if isProductApply(apply) =>
        CCallExpr(CDeclRefExpr(getRecordCreator(apply.tpe, ctx)), l.map(compileTermToCExpr(_, ctx)))
      case Apply(Select(left, "=="), List(right)) if left.tpe <:< TypeRepr.of[Product] =>
        CCallExpr(
          CDeclRefExpr(getRecordEquals(left.tpe, ctx)),
          List(
            compileTermToCExpr(left, ctx),
            compileTermToCExpr(right, ctx)
          )
        )
      case Apply(Select(left, "!="), List(right)) if left.tpe <:< TypeRepr.of[Product] =>
        CNotExpr(
          CCallExpr(
            CDeclRefExpr(getRecordEquals(left.tpe, ctx)),
            List(
              compileTermToCExpr(left, ctx),
              compileTermToCExpr(right, ctx)
            )
          )
        )
      case Apply(Select(qualifier, name), List(_)) if canCompileToCBinaryOperator(qualifier, name) =>
        compileApplyToCBinaryOperator(apply, ctx)
      case _ => throw new MatchError(apply.show(using Printer.TreeStructure))
    }
  }

  def canCompileToCBinaryOperator(using Quotes)(term: quotes.reflect.Term, name: String): Boolean = {
    import quotes.reflect.*

    val binaryArithmeticOperators = List("+", "-", "*", "/", "%")
    val binaryBitwiseLogicalOperators = List("|", "&", "^")
    val binaryBitwiseOperators = binaryBitwiseLogicalOperators ++ List("<<", ">>", ">>>")
    val equalityOperators = List("==", "!=")
    val relationalOperators = equalityOperators ++ List("<", "<=", ">", ">=")
    val binaryLogicalOperators = List("&&", "||")

    val numberBinaryOperators = binaryArithmeticOperators ++ binaryBitwiseOperators ++ relationalOperators
    val booleanBinaryOperators = binaryBitwiseLogicalOperators ++ equalityOperators ++ binaryLogicalOperators

    isNumberType(term.tpe) && numberBinaryOperators.contains(name) ||
      term.tpe <:< TypeRepr.of[Boolean] && booleanBinaryOperators.contains(name)
  }

  def compileApplyToCBinaryOperator(using Quotes)(apply: quotes.reflect.Apply, ctx: TranslationContext): CParenExpr = {
    import quotes.reflect.*

    val Apply(Select(qualifier, name), List(arg)) = apply

    val lhs = compileTermToCExpr(qualifier, ctx)
    val rhs = compileTermToCExpr(arg, ctx)

    name match {
      case "+" if isNumberType(arg.tpe) => CParenExpr(CPlusExpr(lhs, rhs))
      case "-" => CParenExpr(CMinusExpr(lhs, rhs))
      case "*" => CParenExpr(CProdExpr(lhs, rhs))
      case "/" => CParenExpr(CDivExpr(lhs, rhs))
      case "%" => CParenExpr(CModExpr(lhs, rhs))
      case "|" => CParenExpr(CBitwiseOrExpr(lhs, rhs))
      case "&" => CParenExpr(CBitwiseAndExpr(lhs, rhs))
      case "^" => CParenExpr(CBitwiseXorExpr(lhs, rhs))
      case "<<" => CParenExpr(CLeftShiftExpr(lhs, rhs))
      case ">>" => CParenExpr(CRightShiftExpr(lhs, rhs))
      case ">>>" => throw new MatchError(apply.show(using Printer.TreeStructure))
      case "==" => CParenExpr(CEqualsExpr(lhs, rhs))
      case "!=" => CParenExpr(CNotEqualsExpr(lhs, rhs))
      case "<" => CParenExpr(CLessThanExpr(lhs, rhs))
      case "<=" => CParenExpr(CLessEqualsExpr(lhs, rhs))
      case ">" => CParenExpr(CGreaterThanExpr(lhs, rhs))
      case ">=" => CParenExpr(CGreaterEqualsExpr(lhs, rhs))
      case _ => throw new MatchError(apply.show(using Printer.TreeStructure))
    }
  }

  def isNumberType(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean = {
    import quotes.reflect.*

    tpe <:< TypeRepr.of[Byte] ||
      tpe <:< TypeRepr.of[Short] ||
      tpe <:< TypeRepr.of[Char] ||
      tpe <:< TypeRepr.of[Int] ||
      tpe <:< TypeRepr.of[Long] ||
      tpe <:< TypeRepr.of[Float] ||
      tpe <:< TypeRepr.of[Double]
  }

  def isProductApply(using Quotes)(apply: quotes.reflect.Apply): Boolean = {
    import quotes.reflect.*

    apply match {
      case Apply(Select(i, "apply"), _) =>
        (apply.tpe <:< TypeRepr.of[Product]) && (apply.tpe.classSymbol.get == i.tpe.classSymbol.get.companionClass)
      case Apply(TypeApply(Select(i, "apply"), _), _) =>
        (apply.tpe <:< TypeRepr.of[Product]) && (apply.tpe.classSymbol.get == i.tpe.classSymbol.get.companionClass)
      case _ => false
    }
  }

  def compileAssign(using Quotes)(assign: quotes.reflect.Assign, ctx: TranslationContext): CAssignmentExpr = {
    import quotes.reflect.*

    val Assign(lhs, rhs) = assign

    // when is it a dereference on lhs necessary?
    CAssignmentExpr(compileTermToCExpr(lhs, ctx), compileTermToCExpr(rhs, ctx))
  }

  def compileBlockToCStmtExpr(using Quotes)(block: quotes.reflect.Block, ctx: TranslationContext): CStmtExpr = {
    import quotes.reflect.*

    val cCompoundStmt = compileBlockToCCompoundStmt(block, ctx)

    CStmtExpr(cCompoundStmt)
  }

  def compileBlockToCCompoundStmt(using Quotes)(block: quotes.reflect.Block, ctx: TranslationContext): CCompoundStmt = {
    import quotes.reflect.*

    val compiledStatements = block.statements.map(compileStatementToCStmt(_, ctx))

    val stmtList = block.expr.match {
      case Literal(UnitConstant()) => compiledStatements
      case _ => compiledStatements.appended(compileTermToCStmt(block.expr, ctx))
    }

    CCompoundStmt(stmtList)
  }

  def compileBlockToFunctionBody(using Quotes)(block: quotes.reflect.Block, ctx: TranslationContext): CCompoundStmt = {
    import quotes.reflect.*

    val Block(statements, expr) = block

    val compiledStatements = statements.map(compileStatementToCStmt(_, ctx))

    val stmtList = expr match {
      case Literal(UnitConstant()) => compiledStatements
      case _ if expr.tpe =:= TypeRepr.of[Unit] =>
        compiledStatements.appended(compileTermToCStmt(expr, ctx))
      case ret: Return => compiledStatements.appended(compileReturn(ret, ctx))
      case _ => compiledStatements.appended(CReturnStmt(Some(compileTermToCExpr(expr, ctx))))
    }

    CCompoundStmt(stmtList)
  }

  def compileIfToCIfStmt(using Quotes)(ifTerm: quotes.reflect.If, ctx: TranslationContext): CIfStmt = {
    import quotes.reflect.*

    val If(cond, thenp, elsep) = ifTerm

    val condExpr = compileTermToCExpr(cond, ctx)
    val thenpStmt = compileTermToCStmt(thenp, ctx)
    val elsepStmt = elsep match {
      case Literal(UnitConstant()) => None
      case _ => Some(compileTermToCStmt(elsep, ctx))
    }

    CIfStmt(condExpr, thenpStmt, elsepStmt)
  }

  def compileIfToCConditionalOperator(using Quotes)(ifTerm: quotes.reflect.If, ctx: TranslationContext): CConditionalOperator = {
    import quotes.reflect.*

    val If(cond, thenp, elsep) = ifTerm

    val condExpr = compileTermToCExpr(cond, ctx)
    val thenpExpr = compileTermToCExpr(thenp, ctx)
    val elsepExpr = compileTermToCExpr(elsep, ctx)

    CConditionalOperator(condExpr, thenpExpr, elsepExpr)
  }

  def compileReturn(using Quotes)(ret: quotes.reflect.Return, ctx: TranslationContext): CReturnStmt = {
    import quotes.reflect.*

    ret.expr match {
      case Literal(UnitConstant) => CReturnStmt()
      case term => CReturnStmt(Some(compileTermToCExpr(term, ctx)))
    }
  }

  def compileWhile(using Quotes)(whileTerm: quotes.reflect.While, ctx: TranslationContext): CWhileStmt = {
    import quotes.reflect.*

    val While(cond, body) = whileTerm

    val compiledCond = compileTermToCExpr(cond, ctx)

    val compiledBody = body match {
      case block: Block => compileBlockToCCompoundStmt(block, ctx)
      case term => CCompoundStmt(List(compileTermToCStmt(term, ctx)))
    }

    CWhileStmt(compiledCond, compiledBody)
  }

  def compileTypeRepr(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CType = {
    import quotes.reflect.*

    if tpe =:= TypeRepr.of[Boolean] then CBoolType
    else if tpe =:= TypeRepr.of[Byte] then CCharType
    else if tpe =:= TypeRepr.of[Char] then CCharType
    else if tpe =:= TypeRepr.of[Short] then CShortType
    else if tpe =:= TypeRepr.of[Int] then CIntegerType
    else if tpe =:= TypeRepr.of[Long] then CLongType
    else if tpe =:= TypeRepr.of[Float] then CFloatType
    else if tpe =:= TypeRepr.of[Double] then CDoubleType
    else if tpe =:= TypeRepr.of[Unit] then CVoidType
    else if tpe <:< TypeRepr.of[Product] then CRecordType(getRecordDecl(tpe, ctx))
    else throw new MatchError(tpe.show(using Printer.TypeReprStructure))
  }

  def getRecordCreator(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CFunctionDecl = {
    import quotes.reflect.*

    val recName = recordName(tpe)

    ctx.nameToRecordCreator.get(recName) match {
      case Some(decl) => decl
      case None =>
        val recordDecl = getRecordDecl(tpe, ctx)
        val decl = buildRecordCreator(recordDecl)
        ctx.nameToRecordCreator.put(recName, decl)
        decl
    }
  }

  def buildRecordCreator(recordDecl: CRecordDecl): CFunctionDecl = {
    val name = "create_" + recordDecl.name

    val parameters = recordDecl.fields.map {
      case CFieldDecl(name, declaredType) => CParmVarDecl(name, declaredType)
    }

    val returnType = CRecordType(recordDecl)

    val temp = CVarDecl(
      "temp",
      CRecordType(recordDecl),
      Some(CDesignatedInitExpr(
        parameters.map{ p => (p.name, CDeclRefExpr(p)) }
      ))
    )
    val body = CCompoundStmt(List(
      temp,
      CReturnStmt(Some(CDeclRefExpr(temp)))
    ))

    val decl = CFunctionDecl(name, parameters, returnType, Some(body))

    println(decl.textgen)

    decl
  }

  def getRecordEquals(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CFunctionDecl =
    getRecordEquals(getRecordDecl(tpe, ctx), ctx)

  def getRecordEquals(recordDecl: CRecordDecl, ctx: TranslationContext): CFunctionDecl = {
    ctx.nameToRecordEquals.get(recordDecl.name) match {
      case Some(decl) => decl
      case None =>
        val decl = buildRecordEquals(recordDecl, ctx)
        ctx.nameToRecordEquals.put(recordDecl.name, decl)
        decl
    }
  }

  def buildRecordEquals(recordDecl: CRecordDecl, ctx: TranslationContext): CFunctionDecl = {
    val name = "equals_" + recordDecl.name

    val paramLeft = CParmVarDecl("left", CRecordType(recordDecl))
    val paramRight = CParmVarDecl("right", CRecordType(recordDecl))
    val parameters = List(paramLeft, paramRight)

    val returnType = CBoolType

    val comparisons: List[CStmt] = recordDecl.fields.map { f =>
      val cond = f.declaredType.unqualType match {
        case CRecordType(rd) =>
          val eq = getRecordEquals(rd, ctx)
          CNotExpr(
            CCallExpr(
              CDeclRefExpr(eq),
              List(
                CMemberExpr(CDeclRefExpr(paramLeft), f),
                CMemberExpr(CDeclRefExpr(paramRight), f)
              )
            )
          )
        case _ =>
          CNotEqualsExpr(
            CMemberExpr(CDeclRefExpr(paramLeft), f),
            CMemberExpr(CDeclRefExpr(paramRight), f)
          )
      }

      CIfStmt(
        cond,
        CReturnStmt(Some(CFalseLiteral))
      )
    }
    val body = CCompoundStmt(comparisons.appended(CReturnStmt(Some(CTrueLiteral))))

    val decl = CFunctionDecl(name, parameters, returnType, Some(body))

    println(decl.textgen)

    decl
  }

  def getRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CRecordDecl = {
    import quotes.reflect.*

    val recName = recordName(tpe)

    ctx.nameToRecordDecl.get(recName) match {
      case Some(decl) => decl
      case None =>
        val decl = compileTypeReprToCRecordDecl(tpe, ctx)
        ctx.nameToRecordDecl.put(recName, decl)
        decl
    }
  }

  def recordName(using Quotes)(tpe: quotes.reflect.TypeRepr): String = {
    import quotes.reflect.*

    val symbolName = tpe.classSymbol.get.name

    tpe match {
      case AppliedType(_, typeArgs) =>
        val typeArgNames = typeArgs.map { t =>
          if t <:< TypeRepr.of[Product] then recordName(t)
          else t.typeSymbol.name
        }
        symbolName + "_" + typeArgNames.mkString("_")
      case _ => symbolName
    }
  }

  def compileTypeReprToCRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr, ctx: TranslationContext): CRecordDecl = {
    import quotes.reflect.*

    val classSymbol = tpe.classSymbol.get

    val fields = classSymbol.caseFields.collect {
      case symbol if symbol.isValDef =>
        CFieldDecl(symbol.name.strip(), compileTypeRepr(tpe.memberType(symbol), ctx))
    }

    val decl = CRecordDecl(recordName(tpe), fields)

    println(decl.textgen)

    decl
  }
}
