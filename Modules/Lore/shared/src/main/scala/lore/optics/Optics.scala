package lore.optics

import cats.Applicative
import cats.implicits.*
import cats.kernel.Monoid
import lore.ast.*
import monocle.{Fold, Lens, Traversal}

// val traverseAST: Traverse[cats.Id[Term]] = new Traverse[cats.Id[Term]] {
//   def traverse[F[_]: Applicative](as: Term)(f: Term => F[Term]): F[Term] = ???
// }
// def filterKey[K, V](predicate: K => Boolean): Traversal[Map[K, V], V] =
//   new Traversal[Map[K, V], V] {
//     def modifyA[F[_]: Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] =
//       val a = s.map { case (k, v) =>
//         k -> (if (predicate(k)) then f(v) else v.pure[F])
//       }
//       a.sequence
//   }

// def termPlated: Plated[Term] =
//   // look at circe optics implementation
//   new Plated[Term] {

//     override def plate: Traversal[Term, Term] = ???

//   }

val sourcePosLens = Lens[Term, Option[SourcePos]](_.sourcePos)(s => {
  case t @ TViperImport(path, sourcePos, _)      => t.copy(sourcePos = s)
  case t @ TArgT(name, _type, sourcePos, _)      => t.copy(sourcePos = s)
  case t @ TVar(name, sourcePos, _)              => t.copy(sourcePos = s)
  case t @ TAbs(name, _type, body, sourcePos, _) => t.copy(sourcePos = s)
  case t @ TTuple(factors, sourcePos, _)         => t.copy(sourcePos = s)
  case t @ TIf(cond, _then, _else, sourcePos, _) => t.copy(sourcePos = s)
  case t @ TSeq(body, sourcePos, _)              => t.copy(sourcePos = s)
  case t @ TArrow(left, right, sourcePos, _)     => t.copy(sourcePos = s)
  case t @ TTypeAl(name, _type, sourcePos, _)    => t.copy(sourcePos = s)
  case t @ TAssert(body, sourcePos, _)           => t.copy(sourcePos = s)
  case t @ TAssume(body, sourcePos, _)           => t.copy(sourcePos = s)
  case t @ TSource(body, sourcePos, _)           => t.copy(sourcePos = s)
  case t @ TDerived(body, sourcePos, _)          => t.copy(sourcePos = s)
  case t @ TInteraction(
        reactiveType,
        argumentType,
        modifies,
        requires,
        ensures,
        executes,
        sourcePos,
        _
      ) =>
    t.copy(sourcePos = s)
  case t @ TInvariant(condition, sourcePos, _)         => t.copy(sourcePos = s)
  case t @ TNum(value, sourcePos, _)                   => t.copy(sourcePos = s)
  case t @ TDiv(left, right, sourcePos, _)             => t.copy(sourcePos = s)
  case t @ TMul(left, right, sourcePos, _)             => t.copy(sourcePos = s)
  case t @ TAdd(left, right, sourcePos, _)             => t.copy(sourcePos = s)
  case t @ TSub(left, right, sourcePos, _)             => t.copy(sourcePos = s)
  case t @ TTrue(sourcePos, _)                         => t.copy(sourcePos = s)
  case t @ TFalse(sourcePos, _)                        => t.copy(sourcePos = s)
  case t @ TNeg(body, sourcePos, _)                    => t.copy(sourcePos = s)
  case t @ TLt(left, right, sourcePos, _)              => t.copy(sourcePos = s)
  case t @ TGt(left, right, sourcePos, _)              => t.copy(sourcePos = s)
  case t @ TLeq(left, right, sourcePos, _)             => t.copy(sourcePos = s)
  case t @ TGeq(left, right, sourcePos, _)             => t.copy(sourcePos = s)
  case t @ TEq(left, right, sourcePos, _)              => t.copy(sourcePos = s)
  case t @ TIneq(left, right, sourcePos, _)            => t.copy(sourcePos = s)
  case t @ TDisj(left, right, sourcePos, _)            => t.copy(sourcePos = s)
  case t @ TConj(left, right, sourcePos, _)            => t.copy(sourcePos = s)
  case t @ TImpl(left, right, sourcePos, _)            => t.copy(sourcePos = s)
  case t @ TBImpl(left, right, sourcePos, _)           => t.copy(sourcePos = s)
  case t @ TInSet(left, right, sourcePos, _)           => t.copy(sourcePos = s)
  case t @ TForall(vars, triggers, body, sourcePos, _) => t.copy(sourcePos = s)
  case t @ TExists(vars, body, sourcePos, _)           => t.copy(sourcePos = s)
  case t @ TParens(inner, sourcePos, _)                => t.copy(sourcePos = s)
  case t @ TString(value, sourcePos, _)                => t.copy(sourcePos = s)
  case t @ TFCall(parent, field, args, sourcePos, _)   => t.copy(sourcePos = s)
  case t @ TFCurly(parent, field, body, sourcePos, _)  => t.copy(sourcePos = s)
  case t @ TFunC(name, args, sourcePos, _)             => t.copy(sourcePos = s)
})

trait HasChildren[A] {
  extension (a: A) def children: List[Term]
}
given HasChildren[Term] with {
  extension (t: Term) {
    def children: List[Term] = t match {
      case TViperImport(path, sourcePos, _)      => List.empty
      case TArgT(name, _type, sourcePos, _)      => List.empty
      case TVar(name, sourcePos, _)              => List.empty
      case TAbs(name, _type, body, sourcePos, _) => List(body)
      case TTuple(factors, sourcePos, _)         => factors.toList
      case TIf(cond, _then, _else, sourcePos, _) =>
        (List(cond, _then).map(Some(_)) :+ _else).flatten
      case TSeq(body, sourcePos, _)           => body.toList
      case TArrow(left, right, sourcePos, _)  => List(left, right)
      case t: BinaryOp                        => List(t.left, t.right)
      case TTypeAl(name, _type, sourcePos, _) => List.empty
      case TAssert(body, sourcePos, _)        => List(body)
      case TAssume(body, sourcePos, _)        => List(body)
      case TSource(body, sourcePos, _)        => List(body)
      case TDerived(body, sourcePos, _)       => List(body)
      case TInteraction(
            reactiveType,
            argumentType,
            modifies,
            requires,
            ensures,
            executes,
            sourcePos,
            _
          ) =>
        ((requires ++ ensures).map(Some(_)) :+ executes).flatten
      case TInvariant(condition, sourcePos, _) => List(condition)
      case TNum(value, sourcePos, _)           => List.empty
      case TTrue(sourcePos, _)                 => List.empty
      case TFalse(sourcePos, _)                => List.empty
      case TNeg(body, sourcePos, _)            => List(body)
      case TForall(vars, triggers, body, sourcePos, _) =>
        vars.toList ++ triggers.map(_.toList).flatten :+ body
      case TExists(vars, body, sourcePos, _)          => vars.toList :+ body
      case TParens(inner, sourcePos, _)               => List(inner)
      case TString(value, sourcePos, _)               => List.empty
      case TFCall(parent, field, args, sourcePos, _)  => List(parent) ++ args
      case TFCurly(parent, field, body, sourcePos, _) => List(parent) :+ body
      case TFunC(name, args, sourcePos, _)            => args.toList
    }
  }
}

// allows to focus all terms in a certain subtree. Not sure how useful this is and it is much slower than traverseFromNode
val Subtree: Traversal[Term, Term] =
  new Traversal[Term, Term] {
    def modifyA[F[_]](f: Term => F[Term])(s: Term)(using
        F: Applicative[F]
    ): F[Term] =
      Applicative[F].map2(
        f(s),
        s.children.map(modifyA(f)(_)).sequence
      ) {
        case (p: TParens, c: List[Term]) => p.copy(inner = c.head)
        case (p: TNum, _)                => p
        case _                           => ???
      }
  }

val children: Fold[Term, Term] = {
  new Fold[Term, Term] {

    def foldMap[M: Monoid](f: Term => M)(t: Term): M = {
      t match {
        case _: (TViperImport | TArgT | TVar | TTypeAl | TNum | TTrue | TFalse |
            TString) =>
          Monoid[M].empty
        case TAbs(name, _type, body, sourcePos, _) => f(body)
        case x: TTuple =>
          Monoid[M].combineAll(x.factors.toList.map(f))
        case TIf(cond, _then, _else, sourcePos, _) =>
          Monoid[M].combineAll(
            (List(cond, _then).map(Some(_)) :+ _else).flatten.map(f)
          )
        case TSeq(body, sourcePos, _)     => Monoid[M].combineAll(body.toList.map(f))
        case t: BinaryOp                  => f(t.left) combine f(t.right)
        case TAssert(body, sourcePos, _)  => f(body)
        case TAssume(body, sourcePos, _)  => f(body)
        case TSource(body, sourcePos, _)  => f(body)
        case TDerived(body, sourcePos, _) => f(body)
        case TInteraction(
              reactiveType,
              argumentType,
              modifies,
              requires,
              ensures,
              executes,
              sourcePos,
              _
            ) =>
          Monoid[M].combineAll(
            ((requires ++ ensures).map(Some(_)) :+ executes).flatten.map(f)
          )
        case TInvariant(condition, sourcePos, _) => f(condition)
        case TNeg(body, sourcePos, _)            => f(body)
        case TForall(vars, triggers, body, sourcePos, _) =>
          Monoid[M].combineAll(
            (vars.toList ++ triggers.map(_.toList).flatten :+ body).map(f)
          )
        case TExists(vars, body, sourcePos, _) =>
          Monoid[M].combineAll(((vars.toList :+ body).map(f)))
        case TParens(inner, sourcePos, _) => f(inner)
        case TFCall(parent, field, args, sourcePos, _) =>
          Monoid[M].combineAll((List(parent) ++ args).map(f))
        case TFCurly(parent, field, body, sourcePos, _) =>
          Monoid[M].combineAll((List(parent) :+ body).map(f))
        case TFunC(name, args, sourcePos, _) =>
          Monoid[M].combineAll(args.toList.map(f))
      }
    }
  }
}

//   new Traversal[Term, Term] {
//     def modifyA[F[_]: Monad](f: Term => F[Term])(s: Term): F[Term] =
//       val transformed = f(s).map(f)

//       // val b = transformed match
//       //   case y: F[TAbs] =>
//       //     val inner = y.map { case x: TAbs => x.body }

//       //     ???
//       ???

//       // s match
//       //   case t: TNum => f(t)
//       //   case t @ TParens(inner, sourcePos) =>
//       //     val a: F[Term] = f(inner)
//       //     val b = f(t)
//       //     val c: F[Term] = Applicative[F].map2(a, b)((a, b) =>
//       //       b match
//       //         case t: TParens => t.copy(inner = a)
//       //         case _          => ???
//       //     )
//       //     c
//       //   case TAdd(left, right, sourcePos) =>
//       //     (left, right).traverse(f).map(TAdd(_, _, sourcePos))
//       //   case _ => ???
//   }

// val SourcePositions: Traversal[Term, Option[SourcePos]] =
//   new Traversal[Term, Option[SourcePos]] {
//     def modifyA[F[_]: Applicative](f: Term => F[Option[SourcePos]])(
//         s: Term
//     ): F[Option[SourcePos]] =
//       s match
//         case t @ TParens(inner, sourcePos) =>
//           (f(t), modifyA(f)(inner)).traverse
//         case TAdd(left, right, sourcePos) =>
//           (left, right).traverse(f).map(TAdd(_, _, sourcePos))
//         case _ => ???
//   }

// def traverseAST: Traversal[Term, Term] =
//   new Traversal[Term, Term] {
//     def modifyA[F[_]: Applicative](f: Term => F[Term])(s: Term): F[Term] =
//       f(s).map{
//         case t @ TAbs(name, _type, body, sourcePos) => f(body).map(s => t.copy(body=s))
//       }
//         // case t: TTuple(factors, sourcePos)         =>
//         // case t: TIf(cond, _then, _else, sourcePos) =>
//         // case t: TSeq(body, sourcePos)              =>
//         // case t: TArrow(left, right, sourcePos)     =>
//         // case t: TTypeAl(name, _type, sourcePos)    =>
//         // case t: TAssert(body, sourcePos)           =>
//         // case t: TAssume(body, sourcePos)           =>
//         // case t: TSource(body, sourcePos)           =>
//         // case t: TDerived(body, sourcePos)          =>
//         // case t: TInteraction(
//         //      t:  reactiveType,
//         //      t:  argumentType,
//         //      t:  modifies,
//         //      t:  requires,
//         //      t:  ensures,
//         //      t:  executes,
//         //      t:  sourcePos
//         //     )t:  =>
//         // case t: TInvariant(condition, sourcePos)         =>
//         // case t: TDiv(left, right, sourcePos)             =>
//         // case t: TMul(left, right, sourcePos)             =>
//         // case t: TAdd(left, right, sourcePos)             =>
//         // case t: TSub(left, right, sourcePos)             =>
//         // case t: TNeg(body, sourcePos)                    =>
//         // case t: TLt(left, right, sourcePos)              =>
//         // case t: TGt(left, right, sourcePos)              =>
//         // case t: TLeq(left, right, sourcePos)             =>
//         // case t: TGeq(left, right, sourcePos)             =>
//         // case t: TEq(left, right, sourcePos)              =>
//         // case t: TIneq(left, right, sourcePos)            =>
//         // case t: TDisj(left, right, sourcePos)            =>
//         // case t: TConj(left, right, sourcePos)            =>
//         // case t: TImpl(left, right, sourcePos)            =>
//         // case t: TBImpl(left, right, sourcePos)           =>
//         // case t: TInSet(left, right, sourcePos)           =>
//         // case t: TForall(vars, triggers, body, sourcePos) =>
//         // case t: TExists(vars, body, sourcePos)           =>
//         // case t: TParens(inner, sourcePos)                =>
//         // case t: TString(arg, sourcePos)                =>
//         // case t: TFCall(parent, field, args, sourcePos)   =>
//         // case t: TFCurly(parent, field, body, sourcePos)  =>
//         // case t: TFunC(name, args, sourcePos)             =>
//         case t: (TArgT | TVar | TTypeAl | TNum | TTrue | TFalse | TString |
//               TViperImport) =>
//           f(t) // don't traverse in cases without children

//   }
