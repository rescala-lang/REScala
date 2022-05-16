package reactive

import clangast.types.CType
import macros.ScalaToC

import scala.quoted.*

case class Source[V](name: String, cType: CType) extends Event[V] {
  override def inputs: List[ReSource] = Nil

  override val baseName: String = "source"

  override def valueName: String = name
  
  val validName: String = valueName + "_valid"
}

object Source {
  def sourceCode[V](name: Expr[String])(using Quotes, Type[V]): Expr[Source[V]] = {
    import quotes.reflect.*
    
    val tpeCAST = ScalaToC.compileType[V]
    
    '{ Source($name, $tpeCAST) }
  }
  
  inline def apply[V](inline name: String): Source[V] = ${ sourceCode[V]('name) }
}
