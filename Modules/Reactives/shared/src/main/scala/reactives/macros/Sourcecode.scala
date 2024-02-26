package reactives.macros

// Adapted from
// https://github.com/com-lihaoyi/sourcecode/blob/99c30e1d857f9dda962596e450bde4c105e35861/sourcecode/src-3/sourcecode/Macros.scala
// Original license:
/*
The MIT License (MIT)

Copyright (c) 2014 Li Haoyi (haoyi.sg@gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.
 */

import scala.language.implicitConversions
import scala.quoted._

object Sourcecode {

  case class File(value: String)
  object File { inline given generate: File = ${ Macros.fileImpl } }

  case class Line(value: Int)
  object Line { inline given generate: Line = ${ Macros.lineImpl } }

  case class Enclosing(value: String)
  object Enclosing { inline given generate: Enclosing = ${ Macros.enclosingImpl } }

  object Util {
    def isSynthetic(using Quotes)(s: quotes.reflect.Symbol) = isSyntheticName(getName(s))
    def isSyntheticName(name: String) = {
      name == "<init>" || (name.startsWith("<local ") && name.endsWith(">")) || name == "$anonfun" || name == "macro"
    }
    def getName(using Quotes)(s: quotes.reflect.Symbol) = {
      s.name.trim
        .stripSuffix("$") // meh
    }
  }

  object Macros {

    def findOwner(using
        Quotes
    )(owner: quotes.reflect.Symbol, skipIf: quotes.reflect.Symbol => Boolean): quotes.reflect.Symbol = {
      var owner0 = owner
      while (skipIf(owner0)) owner0 = owner0.owner
      owner0
    }

    def actualOwner(using Quotes)(owner: quotes.reflect.Symbol): quotes.reflect.Symbol =
      findOwner(owner, owner0 => Util.isSynthetic(owner0) || Util.getName(owner0) == "ev")

    /** In Scala 3, macro `mcr()` is expanded to:
      *
      * val macro = ...
      * macro
      *
      * Where n is an ordinal. This method returns the first owner that is not
      * such a synthetic variable.
      */
    def nonMacroOwner(using Quotes)(owner: quotes.reflect.Symbol): quotes.reflect.Symbol =
      findOwner(owner, owner0 => { owner0.flags.is(quotes.reflect.Flags.Macro) && Util.getName(owner0) == "macro" })

    private def adjustName(s: String): String =
      // Required to get the same name from dotty
      if (s.startsWith("<local ") && s.endsWith("$>"))
        s.stripSuffix("$>") + ">"
      else
        s

    def fileImpl(using Quotes): Expr[File] = {
      import quotes.reflect._
      val file = quotes.reflect.Position.ofMacroExpansion.sourceFile.getJPath.map(_.toAbsolutePath.toString).getOrElse(
        "unknown path"
      )
      '{ File(${ Expr(file) }) }
    }

    def lineImpl(using Quotes): Expr[Line] = {
      val line = quotes.reflect.Position.ofMacroExpansion.startLine + 1
      '{ Line(${ Expr(line) }) }
    }

    def enclosingImpl(using Quotes): Expr[Enclosing] = {
      import quotes.reflect._
      val path = enclosing(machine = false)(!Util.isSynthetic(_))
      '{ Enclosing(${ Expr(path) }) }
    }

    sealed trait Chunk
    object Chunk {
      case class PkgObj(name: String)       extends Chunk
      case class ClsTrt(name: String)       extends Chunk
      case class ValVarLzyDef(name: String) extends Chunk

    }

    def enclosing(using Quotes)(machine: Boolean)(filter: quotes.reflect.Symbol => Boolean): String = {
      import quotes.reflect._

      var current = Symbol.spliceOwner
      if (!machine)
        current = actualOwner(current)
      else
        current = nonMacroOwner(current)
      var path = List.empty[Chunk]
      while (current != Symbol.noSymbol && current != defn.RootPackage && current != defn.RootClass) {
        if (filter(current)) {

          val chunk = current match {
            case sym if
                  sym.isValDef || sym.isDefDef => Chunk.ValVarLzyDef.apply
            case sym if
                  sym.isPackageDef ||
                  sym.moduleClass != Symbol.noSymbol => Chunk.PkgObj.apply
            case sym if sym.isClassDef => Chunk.ClsTrt.apply
            case _                     => Chunk.PkgObj.apply
          }

          path = chunk(Util.getName(current).stripSuffix("$")) :: path
        }
        current = current.owner
      }
      path.map {
        case Chunk.PkgObj(s)       => adjustName(s) + "."
        case Chunk.ClsTrt(s)       => adjustName(s) + "#"
        case Chunk.ValVarLzyDef(s) => adjustName(s) + " "
      }.mkString.dropRight(1)
    }
  }
}
