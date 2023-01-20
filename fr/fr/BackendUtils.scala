package fr
import AST._
import cats.instances.invariant
import cats.conversions.all
import fr.AST.ParsedExpression
import fr.AST.SourceReactive
import fr.AST.DerivedReactive
import fr.AST.DerivedReactive

def traverseAST(ast: Seq[ParsedExpression], transformer: ParsedExpression => ParsedExpression) = ???

def traverseFromNode[A <: ParsedExpression](node: A, transformer: ParsedExpression => ParsedExpression): A =
    val transformed = transformer(node)
    val result =
        // check if transformer did anything
        if transformed != node then
            transformed
        // if not, go deeper
        else
            transformed match
                case Precondition(index, body) => Precondition(index,
                    traverseFromNode(body, transformer))
                case Postcondition(index, body) => Postcondition(index,
                    traverseFromNode(body, transformer))
                case Invariant(index, body) => Invariant(index, traverseFromNode(body, transformer))
                case Transaction(index, name, reactives, pre, post, args, body) => 
                    Transaction(index, traverseFromNode(name, transformer),
                    reactives.map(_.map(traverseFromNode(_, transformer))),
                    pre.map(traverseFromNode(_, transformer)),
                    post.map(traverseFromNode(_, transformer)),
                    args.map{case (id, typeAnn) => (traverseFromNode(id, transformer), typeAnn)},
                    traverseFromNode(body, transformer))
                case SourceReactive(index, name, body, typeAnn) => SourceReactive(index, 
                    traverseFromNode(name, transformer),
                    traverseFromNode(body, transformer), typeAnn) 
                case Binding(index, name, body, typeAnn) => Binding(index,
                    traverseFromNode(name, transformer),
                    traverseFromNode(body, transformer), typeAnn)
                case Call(index, name, args) => Call(index,
                    traverseFromNode(name, transformer),
                    args.map(traverseFromNode(_, transformer)))
                case MethodCall(index,parent,method,args) => MethodCall(index,
                    traverseFromNode(parent, transformer),
                    traverseFromNode(method, transformer),
                    args.map(traverseFromNode(_, transformer)))
                case Parens(index, inner) => Parens(index, traverseFromNode(inner, transformer))
                case Division(index, l, r) => Division(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Multiplication(index, l, r) => Multiplication(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Addition(index, l, r) => Addition(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Substraction(index, l, r) => Substraction(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case BoolParens(index, inner) => BoolParens(index, traverseFromNode(inner, transformer))
                case Lt(index, l, r) => Lt(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Gt(index, l, r) => Gt(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Leq(index, l, r) => Leq(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Geq(index, l, r) => Geq(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Equality(index, l, r) => Equality(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Inequality(index, l, r) => Inequality(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Disjunction(index, l, r) => Disjunction(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Conjunction(index, l, r) => Conjunction(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Implication(index, l, r) => Implication(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case InSet(index, l, r) => InSet(index,
                    traverseFromNode(l, transformer), traverseFromNode(r, transformer))
                case Forall(index, vars, triggers, body) => Forall(index,
                    vars.map{case (id, typeName) => (traverseFromNode(id, transformer), typeName)},
                    triggers.map(traverseFromNode(_, transformer)),
                    traverseFromNode(body, transformer)
                )
                case AST.Exists(index, vars, body) => AST.Exists(index,
                    vars.map{case (id, typeName) => (traverseFromNode(id, transformer), typeName)},
                    traverseFromNode(body, transformer))
                case e => e // don't traverse in cases without children
    try
        result.asInstanceOf[A]
    catch
        case c: ClassCastException => throw new Exception("AST transformation led to invalid AST.")

def allReactives(ast: Seq[ParsedExpression]): Map[String, Reactive] =
    ast.collect({case r: Reactive => r}).map(r => (r.name.name, r)).toMap

/** Returns the names of all ids that are used throughout the expression.
 */
def uses(e: ParsedExpression): Set[String] = e match
    case Precondition(index, body) => uses(body)
    case Postcondition(index, body) => uses(body)
    case Invariant(index, body) => uses(body)
    case Transaction(index, name, reactives, pre, post, args, body) => 
        reactives.flatten.flatMap(uses).toSet ++ pre.flatMap(uses).toSet ++
        post.flatMap(uses).toSet ++ uses(body)
    case ID(_, name) => Set(name)
    case UnderScore(_) => Set()
    case e: Reactive => uses(e.body) 
    case e: Binding => uses(e.body)
    case Call(index, name, args) => Set(name.name) ++ args.flatMap(uses).toSet
    case MethodCall(index,parent,method,args) => uses(parent) ++ args.flatMap(uses).toSet
    case Number(_,_) => Set()
    case Parens(index, inner) => uses(inner)
    case Division(index, l, r) => uses(l) ++ uses(r)
    case Multiplication(index, l, r) => uses(l) ++ uses(r)
    case Addition(index, l, r) => uses(l) ++ uses(r)
    case Substraction(index, l, r) => uses(l) ++ uses(r)
    case True(_) => Set()
    case False(_) => Set()
    case BoolParens(_, inner) => uses(inner)
    case Lt(index, l, r) => uses(l) ++ uses(r)
    case Gt(index, l, r) => uses(l) ++ uses(r)
    case Leq(index, l, r) => uses(l) ++ uses(r)
    case Geq(index, l, r) => uses(l) ++ uses(r)
    case Equality(index, l, r) => uses(l) ++ uses(r)
    case Inequality(index, l, r) => uses(l) ++ uses(r)
    case Disjunction(index, l, r) => uses(l) ++ uses(r)
    case Conjunction(index, l, r) => uses(l) ++ uses(r)
    case Implication(index, l, r) => uses(l) ++ uses(r)
    case Forall(index, vars, triggers, body) => uses(body)
    case Exists(index, vars, body) => uses(body)
    case e: StringExpr => Set()
    case InSet(index, l,r) => uses(l) ++ uses(r)

def getDependants(reactive: Reactive, ast: Seq[ParsedExpression]): Set[Reactive] =
    val directChildren: Set[DerivedReactive] = ast.collect({
        case d: DerivedReactive if uses(d).contains(reactive.name.name) => d}).toSet
    Set(reactive) ++ directChildren.flatMap(c => getDependants(c, ast))

/** Given the name of a reactive and the registry of all reactives, this function returns all
 * reactives that this one (transitively) depends on.
 */ 
def getSubgraph(reactiveName: String, graph: Map[String, Reactive]): Set[Reactive] =
    val reactive = graph.get(reactiveName)
    reactive match
        case Some(s: SourceReactive) => Set(s)
        case Some(d: DerivedReactive) =>
            val usedReactives = uses(d).filter(name => graph.keys.toSet.contains(name))
            Set(d) ++ usedReactives.flatMap(getSubgraph(_, graph)) 
        case None => throw new Exception(s"Reactive ${reactiveName} not found in AST.")
