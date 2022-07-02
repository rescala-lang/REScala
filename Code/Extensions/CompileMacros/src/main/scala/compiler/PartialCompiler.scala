package compiler

import compiler.context.TranslationContext

import scala.quoted.*

trait PartialCompiler {
  protected inline def ensureCtx[TC <: TranslationContext](using ctx: TranslationContext, cascade: CompilerCascade):
    [A, R] => ((TC, CompilerCascade) ?=> PartialFunction[A, R]) => PartialFunction[A, R] =
      [A, R] => (f: (TC, CompilerCascade) ?=> PartialFunction[A, R]) => ctx match {
        case c: TC => f(using c, cascade)
        case _ => PartialFunction.empty
      }
}

object PartialCompiler {
  inline def ensurePC[PC <: PartialCompiler]:
    [A, R] => (PartialCompiler, PC => PartialFunction[A, R]) => PartialFunction[A, R] =
      [A, R] => (p: PartialCompiler, f: PC => PartialFunction[A, R]) => p match {
        case pc: PC => f(pc)
        case _ => PartialFunction.empty
      }
}
