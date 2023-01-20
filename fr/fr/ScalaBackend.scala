package fr
import AST._
import cats.implicits._
import org.scalafmt.Scalafmt
import org.scalafmt.config.{ScalafmtConfig, Align}
import java.nio.file.Paths

object ScalaBackend:

    type Env = Set[String]
    
    def toRescala(ast: Seq[ParsedExpression]): String =
        val preface = """|import rescala.default._
                         |
                         |object Program {
                         |def main(args: Array[String]): Unit = {""".stripMargin
        val appendix = "}}"

        val program = translateAST(ast)        
        val code = (preface +: program :+ appendix).mkString("\n")
        val style = ScalafmtConfig.default.copy(align = Align.more)
        Scalafmt.format(code=code, style=style, filename="foo.scala", range=Set.empty).get
    
    def toAmm(ast: Seq[ParsedExpression]): String = 
        val preface = """|import $repo.`https://jitpack.io`
                         |import $repo.`http://www.st.informatik.tu-darmstadt.de/maven/` 
                         |import $ivy.`com.github.rescala-lang.rescala::rescala:0923d1786b`
                         |import rescala.default._
                         |""".stripMargin
        val program = translateAST(ast)
        // val scalafmt = Scalafmt.create(this.getClass.getClassLoader).withRespectVersion(false)
        //     .withDefaultVersion("3.0.7")
        val code = (preface +: program).mkString("\n")
        val style = ScalafmtConfig.default.copy(align = Align.more)
        Scalafmt.format(code=code, style=style, filename="foo.sc", range=Set.empty).get
        
        
    def translateAST(ast: Seq[ParsedExpression]): Seq[String] =
        ast.foldl((Set[String](), Seq[String]())){
            case ((reactives, program), expr) =>
                    val (newReactives, scalaExpr) = translateExpr(expr, reactives)
                    (newReactives, program :+ scalaExpr)
            }._2



    def translateExpr(expr: AST.ParsedExpression, reactives: Env): (Env, String) =
        expr match
            case e: Annotation => (reactives, "") // ignore Viper Expressions
            case q: Quantification => throw new Exception(
                s"Illegal use of quantifier $q outside a Viper annotation."
            )
            case Transaction(index, name, _, _, _, args, body) =>
                val argsCompiled = args.map{
                    case (id, Some(typeName)) => s"${id.name}: ${typeName.name}"
                    case (id, None) =>
                        throw new IllegalArgumentException(s"Transaction $name needs arguments for all of its types when being compiled to Scala!")
                }
                (reactives,
                s"def ${name.name}(${argsCompiled.mkString(", ")}) = ${translateExpr(body, reactives)._2}")
            case SourceReactive(index, name, body, typeAnnotation) =>
                val newReactives = reactives + name.name
                val typeAnn = typeAnnotation match
                    case Some(t) => s"[$t]"
                    case _ => ""
                (newReactives,
                s"val ${name.name} = Var$typeAnn(${translateExpr(body, newReactives)._2})")
            case DerivedReactive(index, name, body, typeAnnotation) =>
                val newReactives = reactives + name.name
                val typeAnn = typeAnnotation match
                    case Some(t) => s"[$t]"
                    case _ => ""
                (newReactives,
                s"val ${name.name} = Signal$typeAnn{${translateExpr(body, newReactives)._2}}")
            case ID(index, name) =>
                (reactives,
                if reactives.contains(name) then
                    s"$name()"
                else
                    s"$name")
            case UnderScore(index) => (reactives,"_")
            case Binding(index, name, body, typeAnnotation) =>
                val newReactives = reactives - name.name
                val typeAnn = typeAnnotation match
                    case Some(t) => s": ${t.name}"
                    case _ => ""
                (newReactives,
                s"val ${name.name}$typeAnn = ${translateExpr(body, newReactives)._2}")
            case Call(index, name, args) =>
                val argsCompiled = args.map(translateExpr(_,reactives)._2)
                (reactives,
                s"${name.name}(${argsCompiled.mkString(", ")})")
            case MethodCall(index, parent, method, args) =>
                val argsString = if args.isEmpty then "" else
                    s"(${args.map(translateExpr(_,reactives)._2).mkString(", ")})"
                (reactives,
                s"${translateExpr(parent, reactives)._2}.${method.name}$argsString")
            case Number(index, num) =>
                (reactives,
                s"$num")
            case Parens(index, inner) =>
                (reactives,
                s"(${translateExpr(inner, reactives)._2})")
            case Division(index, left, right) =>
                (reactives,
                s"${translateExpr(left, reactives)._2} / ${translateExpr(right, reactives)._2}")
            case Multiplication(index, left, right) =>
                (reactives,
                s"${translateExpr(left, reactives)._2} * ${translateExpr(right, reactives)._2}")
            case Addition(index, left, right) =>
                (reactives,
                s"${translateExpr(left, reactives)._2} + ${translateExpr(right, reactives)._2}")
            case Substraction(index, left, right) =>
                (reactives,
                s"${translateExpr(left, reactives)._2} - ${translateExpr(right, reactives)._2}")
            case StringExpr(index, content) =>
                (reactives, s"\"$content\"")
            case BoolParens(index, inner) => (reactives, s"(${translateExpr(inner, reactives)})")
            case Conjunction(index, l, r) =>
                (reactives, s"${translateExpr(l, reactives)} && ${translateExpr(r, reactives)}")
            case Disjunction(index, l, r) =>
                (reactives, s"${translateExpr(l, reactives)} || ${translateExpr(r, reactives)}")
            case Equality(index, l, r) =>
                (reactives, s"${translateExpr(l, reactives)} == ${translateExpr(r, reactives)}")
            case Inequality(index, l, r) =>
                (reactives, s"${translateExpr(l, reactives)} != ${translateExpr(r, reactives)}")
            case Implication(index, l, r) =>
                (reactives, s"${translateExpr(l, reactives)} ==> ${translateExpr(r, reactives)}")
            case Gt(index, l, r) =>
                (reactives, s"${translateExpr(l, reactives)} > ${translateExpr(r, reactives)}")
            case Lt(index, l, r) =>
                (reactives, s"${translateExpr(l, reactives)} < ${translateExpr(r, reactives)}")
            case Geq(index, l, r) =>
                (reactives, s"${translateExpr(l, reactives)} >= ${translateExpr(r, reactives)}")
            case Leq(index, l, r) =>
                (reactives, s"${translateExpr(l, reactives)} <= ${translateExpr(r, reactives)}")
            case True(_) => (reactives, "true")
            case False(_) => (reactives, "false")
            case exp => throw new NotImplementedError(s"Expression $exp can not yet be translated to Scala.")