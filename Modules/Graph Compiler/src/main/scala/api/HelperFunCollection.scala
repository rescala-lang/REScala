package api

import clangast.WithContext
import clangast.decl.CFunctionDecl

import scala.collection.mutable

class HelperFunCollection {
  val helperFuns: mutable.Set[WithContext[CFunctionDecl]] = mutable.Set()

  def add(f: WithContext[CFunctionDecl]): WithContext[CFunctionDecl] = {
    helperFuns.add(f)
    f
  }
}
