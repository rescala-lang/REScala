package tests.rescala.fullmv.dynamicgraphviz

import org.graphstream.graph._
import org.graphstream.graph.implementations._

object DynamicGraphViz {
  def main(args: Array[String]): Unit = {
    System.setProperty("org.graphstream.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer");

    val graph = new SingleGraph("Test")
    graph.setAttribute("ui.stylesheet", "node {\n" +
      " text-size: 14;\n" +
      " padding: 5;\n" +
      " stroke-mode: plain;\n" +
      " fill-color: white;\n" +
      " size-mode: fit;\n" +
      " text-alignment: center;\n" +
      "} ")
    //graph.addAttribute("ui.quality");
    graph.addAttribute("ui.antialias");

    graph.addNode[Node]("A").addAttribute("ui.label", "A")
    graph.addNode[Node]("B").addAttribute("ui.label", "B")
    graph.addEdge[Edge]("AB", "A", "B")
    graph.display()
    System.in.read()

    graph.addNode[Node]("C").addAttribute("ui.label", "C")
    System.in.read()

    graph.addEdge[Edge]("AC", "A", "C")
    graph.addEdge[Edge]("BC", "B", "C")
    System.in.read()

    graph.removeNode[Node]("A")
  }
}
