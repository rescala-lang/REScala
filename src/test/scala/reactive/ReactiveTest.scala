package reactive

import clangast.WithContext
import clangast.decl.CFunctionDecl
import macros.{CHelperFun, CHelperFun2}

import java.io.{File, FileWriter}

object ReactiveTest extends App {
  val fun = CHelperFun { (x: Int, y: Int) => x + y }

  println(fun.f.node.textgen)

  fun(0, 1)

  inline def addThree(i: Int): Int = i + 3

  val s = Source[Int]("source_a").map("addThree")(x => addThree(x))
  
  val or = Source[Int]("source_b") || Source[Int]("source_c")
  
  val x = s.filter("greaterThanThree")(_ > 3)

  val o = x.observe("print")(println)

  val m2 = or.map2(x)("mul")(_ * _)

  val sum = m2.fold(math.max(0, 4))("add")(_ + _)

  val tuple = sum.fold((0.1, 0))("makeTuple") {
    case ((acc, _), i) => (acc, i)
  }

  val snap = Source[Int]("source_d").snapshot(tuple)

  val latestSnap = snap.fold((0.1, 0))("last") {
    case (_, latest) => latest
  }

  val gc = new GraphCompiler(List(sum, tuple, latestSnap))

  val f = new File("out/reactifi.c")
  f.createNewFile()
  val fileWriter = new FileWriter(f)
  fileWriter.write(gc.translationUnit.textgen)
  fileWriter.close()
}
