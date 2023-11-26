package cfg_visualiser

/** Generator for fresh node IDs.
  */
object IDGenerator {
  private var current: Int = 0

  def getNewId: Int = {
    current += 1
    current
  }
}

def wrap(input: String, width: Integer = 20): String =
  if (input.length() <= width) {
    input
  } else {
    var splitPoint = width;
    while (input.charAt(splitPoint).isLetterOrDigit && splitPoint > width / 2) {
      // search backwards for a non alphanumeric charcter to split on
      splitPoint -= 1
    }
    if (input.charAt(splitPoint).isLetterOrDigit) {
      // didn't find a character to split on
      splitPoint = width;
    }
    val line = input.substring(0, splitPoint)
    line + "\\l" + wrap(input.substring(splitPoint), width)
  }


/** Super-class for elements of a Graphviz dot file.
  */
abstract class DotElement {

  /** Produces a dot string representation of this element.
    */
  def toDotString: String
}

/** Represents a node in a Graphviz dot file.
  */
class DotNode(val id: String, val label: String) extends DotElement {

  def this(label: String) = this("n" + IDGenerator.getNewId, label)

  def this() = this("")

  def equals(other: DotNode): Boolean = toDotString.equals(other.toDotString)

  override def toString: String = toDotString

  def toDotString: String =
    s"\"$id\"" + "[label=\"" + wrap(label, 80) + "\"]"

}

/** Represents an edge between two nodes in a Graphviz dot file.
  */
class DotArrow(
    val fromNode: DotNode,
    arrow: String,
    val toNode: DotNode,
    val label: String,
    val style: String = "solid",
    val colour: String = "black"
) extends DotElement {

  def equals(other: DotArrow): Boolean = toDotString.equals(other.toDotString)

  def toDotString: String =
    s"\"${fromNode.id}\" $arrow \"${toNode.id}\"[label=\"$label\", style=\"$style\", color=\"$colour\"]"
}

/** Represents a directed edge between two regular cfg nodes in a Graphviz dot file.
  */
class DotRegularArrow(fromNode: DotNode, toNode: DotNode, label: String)
    extends DotArrow(fromNode, "->", toNode, label) {
  def this(fromNode: DotNode, toNode: DotNode) = this(fromNode, toNode, "")

  override def toString: String = super.toDotString
}

/** Represents a directed, inline connection between two cfg nodes in a Graphviz dot file.
  */
class DotInlineArrow(fromNode: DotNode, toNode: DotNode, label: String)
    extends DotArrow(fromNode, "->", toNode, label, style = "dashed", colour = "red") {
  def this(fromNode: DotNode, toNode: DotNode) = this(fromNode, toNode, "")

  override def toString: String = super.toDotString
}

/*
 * Represents a directed, interprocedural edge between two nodes in a Graphviz dot file
 */
class DotInterArrow(fromNode: DotNode, toNode: DotNode, label: String)
    extends DotArrow(fromNode, "->", toNode, label, style = "dashed", colour = "green") {
  def this(fromNode: DotNode, toNode: DotNode) = this(fromNode, toNode, "")

  override def toString: String = super.toDotString
}

/** Represents a directed, intraprocedural cfg edge in a Graphviz dot file.
  */
class DotIntraArrow(fromNode: DotNode, toNode: DotNode, label: String)
    extends DotArrow(fromNode, "->", toNode, label, style = "dashed", colour = "blue") {
  def this(fromNode: DotNode, toNode: DotNode) = this(fromNode, toNode, "")

  override def toString: String = super.toDotString
}

/** Represents a Graphviz dot graph.
  */
class DotGraph(val title: String, val nodes: Iterable[DotNode], val edges: Iterable[DotArrow]) extends DotElement {

  def this(nodes: List[DotNode], edges: List[DotArrow]) = this("", nodes, edges)

  def this(title: String) = this(title, List(), List())

  def this() = this(List(), List())

  def addGraph(g: DotGraph): DotGraph = {
    val ng = g.nodes.foldLeft(this)((g, n) => g.addNode(n))
    g.edges.foldLeft(ng)((g, e) => g.addEdge(e))
  }

  def addNode(n: DotNode): DotGraph =
    if (nodes.exists(a => n.equals(a))) this
    else new DotGraph(title, nodes ++ List(n), edges)

  def addEdge(e: DotArrow): DotGraph =
    if (edges.exists(a => e.equals(a))) this
    else new DotGraph(title, nodes, edges ++ List(e))

  override def toString: String = toDotString

  def toDotString: String = "digraph " + title + " {\n" + (nodes ++ edges).foldLeft("")((str, elm) => str + elm.toDotString + "\n") + "}"
}
