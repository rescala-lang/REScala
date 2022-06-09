package compiler

import scala.annotation.targetName

class CompilerCascade(val partialCompilers: List[PartialCompiler]) {
  given CompilerCascade = this

  @targetName("flowTo")
  def ~~>(other: CompilerCascade): CompilerCascade = new CompilerCascade(this.partialCompilers ++ other.partialCompilers)
  
  @targetName("prepend")
  def ~>:(pc: PartialCompiler): CompilerCascade = new CompilerCascade(pc :: this.partialCompilers)
  
  def dispatch[A, R](f: PartialCompiler => PartialFunction[A, R])(a: A): R = {
    dispatchRec(partialCompilers.map(f), a)
  }
  
  private def dispatchRec[A, R](l: List[PartialFunction[A, R]], a: A): R = {
    l match {
      case Nil => throw new MatchError(a)
      case p :: tail => p.applyOrElse(a, dispatchRec(tail, _))
    }
  }
}

object CompilerCascade {
  def apply(partialCompilers: PartialCompiler*): CompilerCascade = new CompilerCascade(partialCompilers.toList)
}