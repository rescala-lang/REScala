package compiler

import scala.annotation.{tailrec, targetName}

class FragmentedCompiler(val fragments: List[CompilerFragment]) {
  @targetName("append")
  def ++(other: FragmentedCompiler): FragmentedCompiler = new FragmentedCompiler(this.fragments ++ other.fragments)

  @targetName("prepend")
  def +:(fragment: CompilerFragment): FragmentedCompiler = new FragmentedCompiler(fragment +: this.fragments)

  inline def dispatch[S <: CompilerFragment]: [A, R] => (S => PartialFunction[A, R]) => A => R =
    [A, R] =>
      (f: S => PartialFunction[A, R]) =>
        (a: A) =>
          dispatchRec(fragments.collect { case s: S => f(s) }, a).getOrElse(throw new MatchError(a))

  inline def dispatchLifted[S <: CompilerFragment]: [A, R] => (S => PartialFunction[A, R]) => A => Option[R] =
    [A, R] =>
      (f: S => PartialFunction[A, R]) =>
        (a: A) =>
          dispatchRec(fragments.collect { case s: S => f(s) }, a)

  @tailrec
  protected final def dispatchRec[A, R](l: List[PartialFunction[A, R]], a: A): Option[R] = {
    l match {
      case Nil => None
      case p :: tail =>
        a match {
          case p(res) => Some(res)
          case _      => dispatchRec(tail, a)
        }
    }
  }
}

object FragmentedCompiler {
  def apply(fragments: CompilerFragment*): FragmentedCompiler = new FragmentedCompiler(fragments.toList)

  inline def dispatch[S <: CompilerFragment](using
      fs: FragmentedCompiler
  ): [A, R] => (S => PartialFunction[A, R]) => A => R =
    fs.dispatch[S]
}
