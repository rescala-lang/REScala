package compiler

import scala.annotation.{tailrec, targetName}

class CompilerCascade(val partialCompilers: List[PartialCompiler]) {
  given CompilerCascade = this

  @targetName("flowTo")
  def ~~>(other: CompilerCascade): CompilerCascade = new CompilerCascade(this.partialCompilers ++ other.partialCompilers)
  
  @targetName("prepend")
  def ~>:(pc: PartialCompiler): CompilerCascade = new CompilerCascade(pc :: this.partialCompilers)
  
  def dispatch[A, R](f: PartialCompiler => PartialFunction[A, R])(a: A): R = {
    dispatchRec(partialCompilers.map(f), a).getOrElse(throw new MatchError(a))
  }
  
  def dispatchLifted[A, R](f: PartialCompiler => PartialFunction[A, R])(a: A): Option[R] = {
    dispatchRec(partialCompilers.map(f), a)
  }
  
  def dispatchOrElse[A, R](f: PartialCompiler => PartialFunction[A, R])(a: A, orElse: => R): R = {
    dispatchRec(partialCompilers.map(f), a).getOrElse(orElse)
  }
  
  @tailrec
  private def dispatchRec[A, R](l: List[PartialFunction[A, R]], a: A): Option[R] = {
    l match {
      case Nil => None
      case p :: tail =>
        a match {
          case p(res) => Some(res)
          case _ => dispatchRec(tail, a)
        }
    }
  }
}

object CompilerCascade {
  def apply(partialCompilers: PartialCompiler*): CompilerCascade = new CompilerCascade(partialCompilers.toList)
}