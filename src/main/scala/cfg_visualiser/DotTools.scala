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


def escape(s: String) =  {
  val n = s.replace("\"", "\\\"")
  n
}


def wrap(_input: String, width: Integer = 20, first : Boolean = true): String =
  var input = _input

  def cannotSplit(c:Char) = {
    c.isLetterOrDigit || ("_$".contains(c))
  }

  if (input.length() <= width) {
    input.replace("\n", "\\l") + "\\l"
  } else if ({
    val index = input.indexOf('\n')
    index != -1 && index <= width
    }) {
    var splitPoint = input.indexOf('\n')
    val (line, rest) = (input.substring(0, splitPoint).replace("\n", "\\l"), input.substring(splitPoint + 1))
    (if (!first) then "    " else "") + line  + "\\l" + wrap(rest, width=width, true)
  } else {
    var splitPoint = width
    while (cannotSplit(input.charAt(splitPoint)) && splitPoint > width / 3) {
      // search backwards for a non alphanumeric charcter to split on
      splitPoint -= 1
    }
    if (cannotSplit(input.charAt(splitPoint))) {
      // didn't find a character to split on
      splitPoint = width
    }
    val (line, rest) = (input.substring(0, splitPoint).replace("\n", "\\l"), input.substring(splitPoint))
    (if (!first) then "    " else "") + line  + "\\l" + wrap(rest, width=width, false)
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
class DotNode(val id: String, val label: String, highlight: Boolean = false) extends DotElement {

  def this(label: String) = this("n" + IDGenerator.getNewId, label)

  def this() = this("")

  def equals(other: DotNode): Boolean = toDotString.equals(other.toDotString)


  def hl = if (highlight) then "style=filled, fillcolor=\"orangered\", " else ""

  def toDotString: String =
    s"\"$id\"" + s"[${hl}label=\"" + escape(wrap(label, 100)) + "\", shape=\"box\", fontname=\"Mono\", fontsize=\"5\"]"

}

/** Represents an edge between two nodes in a Graphviz dot file.
  */
 case class DotArrow(
    fromNode: DotNode,
    arrow: String,
    toNode: DotNode,
    label: String,
    style: String = "solid",
    colour: String = "black"
) extends DotElement {

  def equals(other: DotArrow): Boolean = toDotString.equals(other.toDotString)


  def toDotString: String =
    s"\"${fromNode.id}\" $arrow \"${toNode.id}\"[label=\"${escape(label)}\", style=\"$style\", color=\"$colour\"]"
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

  val graph = "graph [ fontsize=18 ];" 
  def toDotString: String = "digraph " + title + " {\n" + graph + "\n" + (nodes ++ edges).foldLeft("")((str, elm) => str + elm.toDotString + "\n") + "}"
}



class DotStruct(val id: String, val details: String, val fields: Option[Iterable[String]], val verbose: Boolean = true) extends DotElement {
  def equals(other: DotStruct): Boolean = toDotString.equals(other.toDotString)


  val label = s"\"{<$id> ${if verbose then wrap(details, 80) else id} ${if fields.isDefined then  s" | {${fields.get.map(f => s"<$f> $f").mkString("|")}}" else "" }}\""
  override def toString: String = toDotString

  override def toDotString: String =
    s"$id " + "[label=" + escape(label) + "]"
}

class DotStructElement(val id: String, val field: Option[String]) extends DotElement {
  def equals(other: DotStruct): Boolean = toDotString.equals(other.toDotString)
  override def toString: String = toDotString

  override def toDotString: String =
    s"$id${if field.isDefined then ":" + field.get else ""}"
}

case class StructArrow(
                        from: DotStructElement,
                        to: DotStructElement,
                        label: String = "",
                        arrow: String = "->",
                        style: String = "solid",
                        colour: String = "black") extends DotElement {

  def equals(other: DotArrow): Boolean = toDotString.equals(other.toDotString)

  def toDotString: String =
    s"${from.toString} $arrow ${to.toString} [label=\"${escape(label)}\", style=\"$style\", color=\"$colour\"]"
}


/** Represents a Graphviz dot graph.
 */
class StructDotGraph(val title: String, val nodes: Iterable[DotStruct], val edges: Iterable[StructArrow]) extends DotElement {

  def this(nodes: List[DotStruct], edges: List[StructArrow]) = this("", nodes, edges)

  def this(title: String) = this(title, List(), List())

  def this() = this(List(), List())

  def addGraph(g: StructDotGraph): StructDotGraph = {
    val ng = g.nodes.foldLeft(this)((g, n) => g.addNode(n))
    g.edges.foldLeft(ng)((g, e) => g.addEdge(e))
  }

  def addNode(n: DotStruct): StructDotGraph =
    if (nodes.exists(a => n.equals(a))) this
    else new StructDotGraph(title, nodes ++ List(n), edges)

  def addEdge(e: StructArrow): StructDotGraph =
    if (edges.exists(a => e.equals(a))) this
    else new StructDotGraph(title, nodes, edges ++ List(e))

  override def toString: String = toDotString

  def toDotString: String = "digraph " + title + " {\nrankdir=\"LR\"\nnode [shape=record];\n" + (nodes ++ edges).foldLeft("")((str, elm) => str + elm.toDotString + "\n") + "}"
}

